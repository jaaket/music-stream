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
    return function(onError, onSuccess) {
      var audioContext = audio.audioContext;

      var promise = audioContext.decodeAudioData(arrayBuffer, onSuccess, onError);

      return function (cancelError, cancelerError, cancelerSuccess) {
        // TODO: Cancel
        cancelerSuccess();
      };
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