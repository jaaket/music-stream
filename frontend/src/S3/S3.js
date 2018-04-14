"use strict";

var aws = require('aws-sdk');

var s3 = new aws.S3({
  endpoint: "https://ams3.digitaloceanspaces.com",
  accessKeyId: localStorage.getItem("accessKeyId"),
  secretAccessKey: localStorage.getItem("secretAccessKey")
});

exports._getJsonObject = function (key) {
  return function (onError, onSuccess) {
    var params = {
      Bucket: "music-stream",
      Key: key
    };

    var request = s3.getObject(params, function(err, data) {
      if (err) {
        onError(err);
      } else {
        onSuccess(JSON.parse(data.Body));
      }
    });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
}

exports._getArrayBufferObject = function (key) {
  return function (onError, onSuccess) {
    var params = {
      Bucket: "music-stream",
      Key: key
    };

    var request = s3.getObject(params, function(err, data) {
      if (err) {
        onError(err);
      } else {
        onSuccess(data.Body);
      }
    });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
}