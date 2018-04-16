"use strict";

exports._initAudio = function() {
  var audio = {
    audioContext: new AudioContext(),

    prevVfPtr: 0,

    queue: [],
    playbackStartTime: null,
    nextToSchedule: 0,
    scheduledCount: 0,
    playingBufferDuration: null,
    schedulingEnabled: false,
    pausedOffset: null
  };

  function scheduler(audio) {
    if (audio.schedulingEnabled && audio.queue.length > audio.nextToSchedule && audio.scheduledCount <= 1) {
      var segment = audio.queue[audio.nextToSchedule];
      audio.nextToSchedule += 1;

      var audioContext = audio.audioContext;

      segment.node = audioContext.createBufferSource();
      segment.node.buffer = segment.buffer;
      segment.node.connect(audioContext.destination);

      if (audio.scheduledCount === 0) {
        // TODO: Does playbackStartTime need to be stored separately?
        audio.playbackStartTime = audioContext.currentTime + 0.1;
      } else {
        audio.playbackStartTime = audio.playbackStartTime + audio.playingBufferDuration;
      }

      segment.node.onended = function() {
        audio.scheduledCount -= 1;
        if (!segment.dontFinish) {
          segment.playbackFinished = true;
        }
      };

      segment.dontFinish = false;
      segment.playbackStartTime = audio.playbackStartTime;
      audio.scheduledCount += 1;
      // Take into account piece of segment already played
      audio.playingBufferDuration = segment.buffer.duration - segment.progress;

      segment.node.start(audio.playbackStartTime, segment.progress);
    }

    setTimeout(function() { scheduler(audio); }, 100 );
  }

  scheduler(audio);

  return audio;
}

exports._decode = function(audio) {
  return function(arrayBuffer) {
    return function() {
      var audioContext = audio.audioContext;

      var orig = new Uint8Array(arrayBuffer);
      var buf = Module._malloc(orig.length);
      Module.HEAPU8.set(orig, buf);

      var ptrSize = 4;
      var resultBufPtr = Module._malloc(ptrSize);
      var numSamplesPtr = Module._malloc(ptrSize);
      var resultVfPtrPtr = Module._malloc(ptrSize);
      Module.HEAPU8.fill(0, resultVfPtrPtr, resultVfPtrPtr + ptrSize);
      var res = decodeVorbis(audio.prevVfPtr, buf, orig.length, resultBufPtr, numSamplesPtr, resultVfPtrPtr);
      audio.prevVfPtr = Module.getValue(resultVfPtrPtr, '*');
      var resultBuf = Module.getValue(resultBufPtr, '*');
      var numSamples = Module.getValue(numSamplesPtr, 'i64');
      Module._free(buf); // No longer needed

      // samples are (big-endian) 16-bit signed integers, two channels => multiply by 4
      var resultArray = new Int16Array(Module.HEAPU8.slice(resultBuf, resultBuf + numSamples * 4).buffer);

      // TODO: Get channels, length, sample rate?
      var resultAudioBuf = audioContext.createBuffer(2, numSamples, 44100); // allocate buffers
      for (var c = 0; c < 2; c++) {
        var buffer = resultAudioBuf.getChannelData(c);
        for (var i = 0; i < numSamples; i++) {
          buffer[i] = resultArray[2*i + c] / 32767;
        }
      }

      Module._free(resultBuf);
      Module._free(resultBufPtr);
      Module._free(numSamplesPtr);
      Module._free(resultVfPtrPtr);

      return resultAudioBuf;
    };
  };
}

exports._enqueue = function(audio) {
  return function(audioBuffer) {
    return function() {
      audio.queue.push({
        buffer: audioBuffer,
        node: null, // Initialized when scheduled
        playbackStartTime: null, // Global time
        progress: 0, // How much segment has been played
        playbackFinished: false,
        dontFinish: false // Don't mark as finished when playback ends (useful for pausing)
      });
    };
  };
}

exports._startPlayback = function() {
  return function(audio) {
    return function() {
      audio.schedulingEnabled = true;
    };
  };
}();

exports._pausePlayback = function(audio) {
  return function() {
    audio.schedulingEnabled = false;
    var foundPlaying = false;

    // Stop all scheduled segments
    for (var i = 0; i < audio.queue.length; i++) {
      var segment = audio.queue[i];

      if (segment.node && !segment.playbackFinished) {
        if (!foundPlaying) {
          // This is the currently playing segment
          foundPlaying = true;
          segment.progress += audio.audioContext.currentTime - segment.playbackStartTime;
          audio.nextToSchedule = i;
        }

        segment.dontFinish = true;
        segment.node.stop();
        // TODO: Free node data?
      }
    }
  }
}

exports.queueLength = function(audio) {
  return function() {
    return audio.queue.length - audio.nextToSchedule;
  };
}

exports.dropScheduled = function(audio) {
  return function() {
    var queueLength = audio.queue.length;

    // Stop all scheduled segments
    for (var i = 0; i < queueLength; i++) {
      var segment = audio.queue[i];

      if (segment.node && !segment.playbackFinished) {
        segment.node.stop();
        // TODO: Free node data?
      }
    }

    // Don't reschedule dropped segments
    audio.nextToSchedule = queueLength;
  };
}