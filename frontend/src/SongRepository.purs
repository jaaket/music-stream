module SongRepository where

import Data.Argonaut.Decode
import Data.Generic
import Prelude

import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Either (Either(..))
import Network.HTTP.Affjax (get, AJAX)

import S3

newtype Song = Song {
  uuid :: String,
  title :: String,
  album :: String,
  artist :: String,
  numSegments :: Int
}

instance eqSong :: Eq Song where
  eq (Song {uuid: uuid1}) (Song {uuid: uuid2}) = uuid1 == uuid2

derive instance genericSong :: Generic Song

instance showSong :: Show Song where
  show = gShow

instance decodeJsonSong :: DecodeJson Song where
  decodeJson json = do
    obj <- decodeJson json
    uuid <- obj .? "uuid"
    metadata <- obj .? "metadata"
    title <- metadata .? "title"
    album <- metadata .? "album"
    artist <- metadata .? "artist"
    numSegments <- obj .? "numSegments"
    pure $ Song { uuid, title, album, artist, numSegments }

getSongs :: forall e. Aff (aws :: AWS | e) (Array Song)
getSongs = do
  dbResp <- getJsonObject "database.json"
  case decodeJson dbResp of
      Right db -> pure db
      Left errs ->  throwError (error ("Failed to parse json: " <> show errs))
