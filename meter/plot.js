$(function() {
    var maxPoints = 500;
    var points = [];
    var maxY = 1;

    var plot = $.plot("#plot", [ [] ], {
        series: {
            shadowSize: 0   // Drawing is faster without shadows
        },
        yaxis: {
            min: 0,
            label: "Power (microwatts)",
        },
        xaxis: {
            show: false
        },
        colors: ["#dba255"]
    });

    function format_power(power) {
        if (power >= 1e4) {
            return "" + (power / 1000).toFixed(3) + " mW";
        } else {
            return "" + power.toFixed(3) + " μW";
        }
    }

    var numeric = $("#numeric");
    var socket = new WebSocket('ws://' + location.hostname + ':' + location.port + '/', 'protocolOne');

    socket.onmessage = function (event) {
        sample = JSON.parse(event.data);
        points.push(sample.power / 1e-6);

        // Truncate
        var n = points.length;
        if (n > maxPoints) {
            points = points.slice(n - maxPoints, n);
            n = points.length;
        }

        // update numeric display
        var nAvg = 10;
        var accum = 0;
        for (var i = n - nAvg; i < n; ++i) {
            accum += points[i];
        }
        accum /= nAvg;
        numeric.text(format_power(accum));
    };

    function redraw() {
        // update plot
        var res = [];
        for (var i = 0; i < points.length; ++i) {
            res.push([i, points[i]]);
        }
        plot.setData([res]);
        plot.setupGrid();
        plot.draw();
    }
    window.setInterval(function(event) {redraw();}, 1000/24);

    $(window).on('beforeunload', function(){
        socket.close();
    });
});
