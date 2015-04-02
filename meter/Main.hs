{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent

import Web.Scotty
import qualified Network.WebSockets as WS
import qualified System.ZMQ4 as ZMQ
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Handler.Warp as Warp

import Paths_openpd_meter (getDataFileName)

sampleRate = 33

socketListen :: TChan ByteString -> WS.PendingConnection -> IO ()
socketListen samples pending = do
    conn <- WS.acceptRequest pending
    chan <- atomically $ dupTChan samples
    forever $ do
        sample <- atomically $ readTChan chan
        WS.sendTextData conn sample

pollMeter :: TChan ByteString -> IO ()
pollMeter samples = ZMQ.withContext $ \ctx -> ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
    ZMQ.connect sock "ipc:///tmp/openpd-ttyUSB.openpd"
    forever $ do
        threadDelay (1000*1000 `div` sampleRate)
        ZMQ.send sock [] "\n"
        msg <- ZMQ.receive sock
        atomically $ writeTChan samples msg
        
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
