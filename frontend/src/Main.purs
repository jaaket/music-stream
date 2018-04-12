module Main where

import Audio
import Prelude
import SongRepository

import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (get, AJAX)


type State = {
  playing :: Boolean,
  songs :: Array Song,
  playlist :: Array Song
}

data Query a
  = Toggle a
  | IsPlaying (Boolean -> a)
  | FetchSongs a
  | AddToPlaylist Song a
  | RemoveFromPlaylist Song a

player :: forall e. Audio -> H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX, audio :: AUDIO | e))
player audio =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action FetchSongs)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = {
    playing: false,
    songs: [],
    playlist: []
  }

  renderSong :: forall a. (Song -> Unit -> Query Unit) -> Song -> H.ComponentHTML Query
  renderSong clickHandler song@(Song {title, album, artist}) =
    HH.div
      [ HP.class_ (H.ClassName "song-list__song")
      , HE.onClick (HE.input_ (clickHandler song))
      ]
      [ HH.div_ [ HH.text title ]
      , HH.div_ [ HH.text album ]
      , HH.div_ [ HH.text artist ]
      ]

  renderSongList :: Array Song -> H.ComponentHTML Query
  renderSongList songs =
    HH.div [ HP.class_ (H.ClassName "song-list") ]
      (map (renderSong AddToPlaylist) songs)

  renderPlaylist :: Array Song -> H.ComponentHTML Query
  renderPlaylist songs =
    HH.div [ HP.class_ (H.ClassName "song-list") ]
      (map (renderSong RemoveFromPlaylist) songs)

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state.playing then "⏸" else "▶"
    in
      HH.div [HP.class_ (H.ClassName "main")]
        [
          renderSongList (state.songs),
          HH.button
            [ HP.title label
            , HE.onClick (HE.input_ Toggle)
            ]
            [ HH.text label ],
          renderPlaylist (state.playlist)
        ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX, audio :: AUDIO | e))
  eval = case _ of
    Toggle next -> do
      playing <- H.gets _.playing
      let nextPlaying = not playing
      if nextPlaying
        then H.liftEff (startPlayback audio)
        else H.liftEff (pausePlayback audio)
      H.modify (\s -> s { playing = nextPlaying })
      pure next
    IsPlaying reply -> do
      playing <- H.gets _.playing
      pure (reply playing)
    FetchSongs next -> do
      songs <- H.liftAff getSongs
      H.modify (\s -> s { songs = songs })
      pure next
    AddToPlaylist song next -> do
      H.modify (\s -> s { playlist = s.playlist <> [song]})
      pure next
    RemoveFromPlaylist song next -> do
      H.modify (\s -> s { playlist = filter (_ /= song) s.playlist})
      pure next

nextAudioSegmentId :: Int -> Int
nextAudioSegmentId prevId = prevId + 1

fillQueue :: forall e. Audio -> Int -> Aff (ajax :: AJAX, audio :: AUDIO | e) Unit
fillQueue audio prevId = do
  len <- liftEff (queueLength audio)
  if len < 5
    then do
      let nextId = nextAudioSegmentId prevId
      res <- get ("http://localhost:8099/get/out" <> show nextId <> ".ogg")
      audioData <- liftEff (decode audio (res.response))
      liftEff (enqueue audio audioData)
      delay (Milliseconds 500.0)
      fillQueue audio nextId
    else do
      delay (Milliseconds 500.0)
      fillQueue audio prevId

main :: Eff (ajax :: AJAX, audio :: AUDIO, console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  audio <- H.liftEff initAudio
  _ <- forkAff (fillQueue audio 0)
  runUI (player audio) unit body
