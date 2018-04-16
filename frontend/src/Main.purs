module Main where

import Audio
import Prelude
import S3
import SongRepository

import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (filter, index, length)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)


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
  nextToSchedule :: Maybe SegmentIdx
}

data Query a
  = Toggle a
  | NextSong a
  | Init a
  | AddToPlaylist Song a
  | RemoveFromPlaylist Song a

player :: forall e. Audio -> H.Component HH.HTML Query Unit Void (Aff (aws :: AWS, console :: CONSOLE, audio :: AUDIO | e))
player audio =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = {
    songs: [],
    playlist: [],
    playback: Paused,
    nextToSchedule: Nothing
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
    HH.div [HP.class_ (H.ClassName "main")]
      [
        renderSongList (state.songs),
        HH.button
          []
          [ HH.text "⏮" ],
        HH.button
          [ HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text  if isPlaying state.playback then "⏸" else "▶" ],
        HH.button
          [ HE.onClick (HE.input_ NextSong) ]
          [ HH.text "⏭" ],
        renderPlaylist (state.playlist)
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (aws :: AWS, console :: CONSOLE, audio :: AUDIO | e))
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
    NextSong next -> do
      H.liftEff (dropScheduled audio)
      playlist <- H.gets _.playlist
      currentSongIdxMaybe <- H.gets (_.nextToSchedule >>> map _.songIdx) -- TODO: Get playing song
      let nextSegmentIdx = currentSongIdxMaybe >>= nextSongFirstSegment playlist
      H.modify (\s -> s { nextToSchedule = nextSegmentIdx })
      pure next
    Init next -> do
      _ <- H.fork fillQueue
      songs <- H.liftAff getSongs
      H.modify (\s -> s { songs = songs })
      pure next
    AddToPlaylist song next -> do
      playlist <- H.gets _.playlist
      when (length playlist == 0) $ do
        H.modify (\s -> s { nextToSchedule = Just ({ songIdx: 0, songSegmentIdx: 1 }) })
      H.modify (\s -> s { playlist = playlist <> [song] })
      pure next
    RemoveFromPlaylist song next -> do
      H.modify (\s -> s { playlist = filter (_ /= song) s.playlist})
      pure next

  fillQueue :: forall f. H.ComponentDSL State Query Void (Aff (aws :: AWS, console :: CONSOLE, audio :: AUDIO | f)) Unit
  fillQueue = do
    len <- H.liftEff (queueLength audio)
    nextId <- if len < 5
      then do
        playlist <- H.gets _.playlist
        nextToSchedule <- H.gets _.nextToSchedule
        -- TODO: Next song pre-fetching
        case nextToSchedule >>= segmentId playlist of
          Just toScheduleId -> do
            buf <- H.liftAff (getArrayBufferObject (toScheduleId <> ".ogg"))
            audioData <- H.liftEff (decode audio buf)
            H.liftEff (enqueue audio audioData)
            pure unit
          Nothing -> pure unit
        H.modify (\s -> s { nextToSchedule = nextToSchedule >>= nextSegment playlist })
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

nextSongFirstSegment :: Array Song -> Int -> Maybe SegmentIdx
nextSongFirstSegment playlist songIdx =
  if songIdx + 1 < length playlist
    then Just { songIdx: songIdx + 1, songSegmentIdx: 1 }
    else Nothing

segmentId :: Array Song -> SegmentIdx -> Maybe String
segmentId playlist { songIdx, songSegmentIdx } =
  map
    (\(Song { uuid }) -> uuid <> "-" <> show songSegmentIdx)
    (index playlist songIdx)

main :: Eff (aws :: AWS, audio :: AUDIO, console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  audio <- H.liftEff initAudio
  runUI (player audio) unit body
