{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad (forever, mzero)
import Data.Monoid ((<>), mempty)
import Control.Applicative

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import GHC.Generics

import Web.Scotty
import qualified Network.WebSockets as WS
import qualified System.ZMQ4 as ZMQ
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Handler.Warp as Warp

import Paths_openpd_meter (getDataFileName)

sampleRate = 33 -- per second

data Sample = Sample { range, power :: Double }
            deriving (Show, Read, Eq, Ord, Generic)
instance Aeson.FromJSON Sample
instance Aeson.ToJSON Sample

newtype DeviceList = DeviceList { devices :: [Text] }
                   deriving (Show, Read, Eq, Ord, Generic)
instance Aeson.FromJSON DeviceList

command :: Aeson.FromJSON a => ZMQ.Socket ZMQ.Req -> String -> Aeson.Object -> IO a
command sock reqType args = do
    ZMQ.send sock [] $ LBS.toStrict $ Aeson.encode
      $ Aeson.toJSON $ HM.insert "type" (Aeson.toJSON reqType) args

    Just msg <- Aeson.decode . LBS.fromStrict <$> ZMQ.receive sock

    let --reply :: Aeson.Object -> Aeson.Parser (Either String a)
        reply obj = (obj .: "error") <|> Aeson.parseJSON (Aeson.Object obj)
    Just resp <- return $ Aeson.parseMaybe reply msg
    return resp

socketListen :: TChan Sample -> WS.PendingConnection -> IO ()
socketListen samples pending = do
    conn <- WS.acceptRequest pending
    chan <- atomically $ dupTChan samples
    forever $ do
        sample <- atomically $ readTChan chan
        WS.sendTextData conn $ Aeson.encode sample

pollMeter :: TChan Sample -> IO ()
pollMeter samples = ZMQ.withContext $ \ctx -> ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
    ZMQ.connect sock "tcp://127.0.0.1:9276"
    dev:_ <- devices <$> command sock "list-devices" mempty
    forever $ do
        threadDelay (1000*1000 `div` sampleRate)
        msg <- command sock "sample" (HM.fromList ["device" .= Aeson.String dev])
        atomically $ writeTChan samples msg

main :: IO ()
main = do
    samples <- newBroadcastTChanIO
    async $ pollMeter samples
    indexHtml <- getDataFileName "index.html"
    plotJs <- getDataFileName "plot.js"
    app <- scottyApp $ do
        get "/" $ file indexHtml
        get "/plot.js" $ file plotJs
    let connOpts = WS.defaultConnectionOptions
    let port = 3000
    putStrLn $ "Running on port "++show port
    Warp.run port $ websocketsOr connOpts (socketListen samples) app
