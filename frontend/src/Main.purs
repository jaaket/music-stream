module Main where

import Audio
import Prelude
import SongRepository

import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (filter, index, length)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (get, AJAX)


data PlaybackState
  = Paused
  | Playing

isPlaying :: PlaybackState -> Boolean
isPlaying Playing = true
isPlaying Paused = false

type SegmentIdx = {
  songIdx :: Int,
  songSegmentIdx :: Int
}

type State = {
  songs :: Array Song,
  playlist :: Array Song,
  playback :: PlaybackState,
  lastScheduled :: SegmentIdx
}

data Query a
  = Toggle a
  | FetchSongs a
  | AddToPlaylist Song a
  | RemoveFromPlaylist Song a

player :: forall e. Audio -> H.Component HH.HTML Query Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | e))
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
    songs: [],
    playlist: [],
    playback: Paused,
    lastScheduled: { songIdx: 0, songSegmentIdx: 0 }
  }

  renderSong :: forall a. (Song -> Unit -> Query Unit) -> Song -> H.ComponentHTML Query
  renderSong clickHandler song@(Song {title, album, artist}) =
    HH.div
      [ HP.class_ (H.ClassName "song-list__song")
      , HE.onClick (HE.input_ (clickHandler song))
      ]
      [ HH.div [ HP.class_ (H.ClassName "song-list__song-title") ] [ HH.text title ]
      , HH.div [ HP.class_ (H.ClassName "song-list__song-album") ] [ HH.text album ]
      , HH.div [ HP.class_ (H.ClassName "song-list__song-artist") ] [ HH.text artist ]
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
      label = if isPlaying state.playback then "⏸" else "▶"
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | e))
  eval = case _ of
    Toggle next -> do
      playback <- H.gets _.playback
      case playback of
        Paused -> do
          H.liftEff (startPlayback audio)
          H.modify (\s -> s { playback = Playing })
        Playing -> do
          H.liftEff (pausePlayback audio)
          H.modify (\s -> s { playback = Paused })
      pure next
    FetchSongs next -> do
      _ <- H.fork fillQueue
      songs <- H.liftAff getSongs
      H.modify (\s -> s { songs = songs })
      pure next
    AddToPlaylist song next -> do
      H.modify (\s -> s { playlist = s.playlist <> [song] })
      pure next
    RemoveFromPlaylist song next -> do
      H.modify (\s -> s { playlist = filter (_ /= song) s.playlist})
      pure next

  fillQueue :: forall f. H.ComponentDSL State Query Void (Aff (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | f)) Unit
  fillQueue = do
    len <- H.liftEff (queueLength audio)
    nextId <- if len < 5
      then do
        playlist <- H.gets _.playlist
        lastScheduled <- H.gets _.lastScheduled
        let nextMaybe = do
                          next <- nextSegment playlist lastScheduled
                          nextId <- segmentId playlist next
                          pure (Tuple next nextId)
        case nextMaybe of
          Just (Tuple next nextId) -> do
            res <- H.liftAff (get ("http://localhost:8099/get/" <> nextId <> ".ogg"))
            audioData <- H.liftEff (decode audio (res.response))
            H.liftEff (enqueue audio audioData)
            H.modify (\s -> s { lastScheduled = next })
            pure unit
          Nothing -> pure unit
      else pure unit
    H.liftAff (delay (Milliseconds 500.0))
    fillQueue

nextSegment :: Array Song -> SegmentIdx -> Maybe SegmentIdx
nextSegment playlist { songIdx, songSegmentIdx } =
  map
    (\(Song song) ->
      if songSegmentIdx >= song.numSegments
        then { songIdx: songIdx + 1, songSegmentIdx: 1 }
        else { songIdx: songIdx, songSegmentIdx: songSegmentIdx + 1 })
    (index playlist songIdx)

segmentId :: Array Song -> SegmentIdx -> Maybe String
segmentId playlist { songIdx, songSegmentIdx } =
  map
    (\(Song { uuid }) -> uuid <> "-" <> show songSegmentIdx)
    (index playlist songIdx)

main :: Eff (ajax :: AJAX, audio :: AUDIO, console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  audio <- H.liftEff initAudio
  runUI (player audio) unit body
