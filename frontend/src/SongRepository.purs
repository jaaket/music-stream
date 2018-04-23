module SongRepository where

import Data.Argonaut.Decode
import Data.Generic
import Prelude
import S3

import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Network.HTTP.Affjax (get, AJAX)

newtype Song = Song {
  uuid :: String,
  title :: String,
  album :: String,
  artist :: String,
  numSegments :: Int,
  track :: Maybe Int
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
    trackStrMaybe :: Maybe String <- metadata .? "track" --
    let track = trackStrMaybe >>= \trackStr ->
          case split (Pattern "/") trackStr of
            [track, total] -> fromString track
            [track] -> fromString track
            _ -> Nothing
    numSegments <- obj .? "numSegments"
    pure $ Song { uuid, title, album, artist, numSegments, track }

getSongs :: forall e. Aff (aws :: AWS | e) (Array Song)
getSongs = do
  dbResp <- getJsonObject "database.json"
  case decodeJson dbResp of
      Right db -> pure db
      Left errs ->  throwError (error ("Failed to parse json: " <> show errs))
