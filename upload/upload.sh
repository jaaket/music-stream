#!/usr/bin/env bash

set -eo pipefail

if [[ -z "$1" ]]; then
  echo "Usage: upload.sh file"
  exit 1
fi

WORK_DIR=$(mktemp -d)

if [[ ! "$WORK_DIR" || ! -d "$WORK_DIR" ]]; then
  echo "Could not create temp dir"
  exit 1
fi

function cleanup {
  echo "Cleaning up"
  rm -rf "$WORK_DIR"
}

trap cleanup EXIT

SONG_ID=$(uuidgen)
if [[ ! $? ]]; then
  echo "Failed to generate song id"
  exit 1
fi

# Assume a single song for now
METADATA_FILE="$WORK_DIR"/metadata.txt
ffmpeg -i "$1" -f ffmetadata "$METADATA_FILE"
if [[ ! $? ]]; then
  echo "Failed to get song metadata"
  exit 1
fi

TITLE=$(grep -ie '^title' < $METADATA_FILE | cut -d'=' -f2)
ALBUM=$(grep -ie '^album' < $METADATA_FILE | cut -d'=' -f2)
ARTIST=$(grep -ie '^artist' < $METADATA_FILE | cut -d'=' -f2)
echo "$TITLE"
echo "$ALBUM"
echo "$ARTIST"

sox "$1" -C 7 "$WORK_DIR"/"$SONG_ID"-%1n.ogg trim 0 15 : newfile : restart
cd "$WORK_DIR"
aws --endpoint-url https://ams3.digitaloceanspaces.com s3 cp . s3://music-stream/ --recursive