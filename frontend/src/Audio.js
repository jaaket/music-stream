"use strict";

exports._initAudio = function() {
  var audioContext = new AudioContext();

  return {
    audioContext: audioContext,
    sourceNode: null
  };
}

exports._decode = function(audio) {
  return function(arrayBuffer) {
    return function() {
      var audioContext = audio.audioContext;

      var orig = new Uint8Array(arrayBuffer);
      var buf = Module._malloc(orig.length);
      Module.HEAPU8.set(orig, buf);
      console.log(orig.length);

      var ptrSize = 4;
      var resultBufPtr = Module._malloc(ptrSize);
      var numSamplesPtr = Module._malloc(ptrSize);
      var res = decodeVorbis(buf, orig.length, resultBufPtr, numSamplesPtr);
      var resultBuf = Module.getValue(resultBufPtr, '*');
      var numSamples = Module.getValue(numSamplesPtr, 'i64');
      console.log(res);
      console.log(resultBuf);
      console.log(numSamples);
    
      Module._free(buf); // No longer needed

      // samples are (big-endian) 16-bit signed integers, two channels => multiply by 4
      var resultArray = new Int16Array(Module.HEAPU8.slice(resultBuf, resultBuf + numSamples * 4).buffer);

      console.log(resultArray);

      // TODO: Get channels, length, sample rate?
      // var numSamples = resultBufLen / 2 / 2; // div by channels and 2 bytes
      var resultAudioBuf = audioContext.createBuffer(2, numSamples, 48000); // allocate buffers
      for (var c = 0; c < 2; c++) {
        var buffer = resultAudioBuf.getChannelData(c);
        for (var i = 0; i < numSamples; i++) {
          buffer[i] = resultArray[2*i + c] / 32767;
        }
      }
//      var promise = audioContext.decodeAudioData(arrayBuffer, onSuccess, onError);

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
        console.log(audioBuffer);
        var audioContext = audio.audioContext;
        var sourceNode = audioContext.createBufferSource();
        sourceNode.buffer = audioBuffer;
        sourceNode.connect(audioContext.destination);
        sourceNode.start(time);
      };
    };
  };
}

exports._stopPlayback = function(audio) {
  return function() {
    audio.sourceNode.stop();
  }
}