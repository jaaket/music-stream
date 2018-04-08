"use strict";

exports._initAudio = function() {
  var audioContext = new AudioContext();

  return {
    audioContext: audioContext,
    playbackStartTime: null,
    prevVfPtr: 0,
    sourceNode: null,
    queue: [],
    scheduledCount: 0,
    playingBufferDuration: null
  };
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

exports.duration = function(audioBuffer) {
  return audioBuffer.duration;
}

exports.bufferLength = function(audioBuffer) {
  return audioBuffer.length;
}

exports._schedule = function(audio) {
  return function(audioBuffer) {
    return function(time) {
      return function() {
        var audioContext = audio.audioContext;
        var sourceNode = audioContext.createBufferSource();
        sourceNode.buffer = audioBuffer;
        sourceNode.connect(audioContext.destination);
        sourceNode.start(time);
      };
    };
  };
}

exports._enqueue = function(audio) {
  return function(audioBuffer) {
    return function() {
      audio.queue.push(audioBuffer);
    };
  };
}

exports._startPlayback = function() {
  function popAndPlay(audio) {
    if (audio.queue.length > 0) {
      if (audio.scheduledCount <= 1) {
        var audioBuffer = audio.queue.shift();
        var audioContext = audio.audioContext;

        audio.sourceNode = audioContext.createBufferSource();
        audio.sourceNode.buffer = audioBuffer;
        audio.sourceNode.connect(audioContext.destination);

        if (audio.scheduledCount === 0) {
          audio.playbackStartTime = audioContext.currentTime + 0.1;
        } else {
          audio.playbackStartTime = audio.playbackStartTime + audio.playingBufferDuration;
        }

        audio.sourceNode.onended = function() {
          audio.scheduledCount -= 1;
        };
        audio.sourceNode.start(audio.playbackStartTime);
        audio.scheduledCount += 1;
        audio.playingBufferDuration = audioBuffer.duration;
      }
    }

    setTimeout(function() { popAndPlay(audio); }, 100 );
  }

  return function(audio) {
    return function() {
      popAndPlay(audio);
    };
  };
}();

exports._pausePlayback = function(audio) {
  return function() {
    audio.sourceNode.stop();
  }
}

exports.queueLength = function(audio) {
  return function() {
    return audio.queue.length;
  };
}