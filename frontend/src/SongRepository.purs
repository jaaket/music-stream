module SongRepository where

import Data.Argonaut.Decode
import Prelude

import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Network.HTTP.Affjax (get, AJAX)

newtype Song = Song {
  uuid :: String,
  title :: String,
  album :: String,
  artist :: String
}

instance decodeJsonSong :: DecodeJson Song where
  decodeJson json = do
    obj <- decodeJson json
    uuid <- obj .? "uuid"
    metadata <- obj .? "metadata"
    title <- metadata .? "title"
    album <- metadata .? "album"
    artist <- metadata .? "artist"
    pure $ Song { uuid, title, album, artist }

getSongs :: forall e. Aff (ajax :: AJAX | e) (Array Song)
getSongs = do
  dbResp <- get "http://localhost:8099/get/database.json"
  case decodeJson (dbResp.response) of
      Right db -> pure db
      Left errs ->  throwError (error ("Failed to parse json: " <> show errs))
