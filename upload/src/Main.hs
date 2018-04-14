{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.AList as AList
import qualified Data.Ini.List as Ini
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)
import GHC.Generics (Generic)


parser :: Parser FilePath
parser = argPath "file"  "File to upload"

data Metadata = Metadata {
  title :: T.Text,
  album :: T.Text,
  artist :: T.Text
} deriving (Generic)

data Song = Song {
  uuid :: T.Text,
  numSegments :: Int,
  metadata :: Metadata
} deriving (Generic)

instance ToJSON Metadata
instance FromJSON Metadata
instance ToJSON Song
instance FromJSON Song

readMetadata :: FilePath -> FilePath -> IO Metadata
readMetadata workDir file = do
  let metadataPath = workDir <> "metadata.ini"
  proc "ffmpeg" ["-i", format fp file, "-f", "ffmetadata", format fp metadataPath] empty
  Just metadata <- liftIO (Ini.parseFile (encodeString metadataPath))
  let (Just title) :: Maybe String = Ini.get metadata Nothing "title"
  let (Just album) :: Maybe String = Ini.get metadata Nothing "album"
  let (Just artist) :: Maybe String = Ini.get metadata Nothing "artist"
  return $ Metadata (T.pack title) (T.pack album) (T.pack artist)

upload :: FilePath -> Shell ()
upload file = do
  workDir <- using (mktempdir "/tmp" "music-upload")
  uuid <- UUID.toText <$> liftIO UUID.nextRandom
  metadata <- liftIO $ readMetadata workDir file
  let targetFiles = format fp (workDir <> fromText (uuid <> "-%1n.ogg"))
  procs "sox" [format fp file, targetFiles, "trim", "0", "15", ":", "newfile", ":", "restart"] empty
  cd workDir
  numSegments <- fold (fmap (unsafeTextToLine . T.pack . encodeString) (find (suffix ".ogg") "")) countLines
  let song = Song uuid numSegments metadata

  procs "aws" ["--endpoint-url", "https://ams3.digitaloceanspaces.com", "s3", "cp", "s3://music-stream/database.json", "."] empty
  Just db :: Maybe [Song] <- decode <$> liftIO (LB.readFile "database.json")
  let newDb = db <> [song]
  liftIO (LB.writeFile "database.json" (encode newDb))
  procs "aws" ["--endpoint-url", "https://ams3.digitaloceanspaces.com", "s3", "cp", "database.json", "s3://music-stream/database.json"] empty
  procs "aws" ["--endpoint-url", "https://ams3.digitaloceanspaces.com", "s3", "cp", ".", "s3://music-stream/", "--recursive", "--exclude", "*", "--include", "*.ogg"] empty
  return ()

main :: IO ()
main = do
  file <- options "Upload" parser
  sh (upload file)
