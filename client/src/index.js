'use strict';

// Require index.html so it gets copied to dist
require('./index.html')

$('.ui.radio.checkbox')
  .checkbox()
;

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);


app.ports.setCurrentTime.subscribe(function(time) {
    var video = document.getElementById('video-player');
    video.currentTime = time;
})

app.ports.togglePlaying.subscribe(function(_) {
    var video = document.getElementById('video-player');
    if (video.paused)
        video.play();
    else
        video.pause();
})

app.ports.startFrameUpdater.subscribe(function(_) {
    var video = document.getElementById('video-player');
    var lastTime = 0
    function draw() {
        var time = video.currentTime;
        if (time !== lastTime) {
            //console.log('time: ' + time);
            //todo: do your rendering here
            lastTime = time;
            app.ports.getCurrentTime.send(time);
        }    
        //wait approximately 16ms and run again
        requestAnimationFrame(draw);
    }
    draw();
});

app.ports.getVideoMeta.subscribe(function() {
    var video = document.getElementById('video-player');
    video.onloadedmetadata = function() {
        app.ports.videoMeta.send([video.duration, video.videoWidth, video.videoHeight]);
    }
})