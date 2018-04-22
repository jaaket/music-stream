module Main where

import Audio
import Prelude
import S3
import SongRepository

import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, warn)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?))
import Data.Argonaut.Core as Json
import Data.Array (dropWhile, filter, find, index, length, take)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Halogen (liftEff)
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

type EntryId = Int

data PlaylistSegmentIdx = PlaylistSegmentIdx {
  entryId :: EntryId,
  segmentWithinEntryIdx :: Int
}

data SongSegmentIdx = SongSegmentIdx {
  songUuid :: String,
  segmentWithinSongIdx :: Int
}

derive instance eqSongSegmentIdx :: Eq SongSegmentIdx
derive instance ordSongSegmentIdx :: Ord SongSegmentIdx

type PlaylistEntry = {
  entryId :: EntryId,
  song :: Song
}

type Playlist = Array PlaylistEntry

instance decodeJsonPlaylistSegmentIdx :: DecodeJson PlaylistSegmentIdx where
  decodeJson json = do
    obj <- decodeJson json
    entryId <- obj .? "entryId"
    segmentWithinEntryIdx <- obj .? "segmentWithinEntryIdx"
    pure $ PlaylistSegmentIdx { entryId: entryId, segmentWithinEntryIdx: segmentWithinEntryIdx }

instance encodeJsonPlaylistSegmentIdx :: EncodeJson PlaylistSegmentIdx where
  encodeJson (PlaylistSegmentIdx { entryId, segmentWithinEntryIdx }) =
    Json.fromObject $
      StrMap.fromFoldable [
        Tuple "entryId" (Json.fromNumber (toNumber entryId)),
        Tuple "segmentWithinEntryIdx" (Json.fromNumber (toNumber segmentWithinEntryIdx))
      ]

type State = {
  songs :: Array Song,
  playlist :: Playlist,
  playback :: PlaybackState,
  nextToSchedule :: Maybe PlaylistSegmentIdx,
  audioCache :: Map SongSegmentIdx AudioBuffer,
  idGenState :: Int
}

data Query a
  = Toggle a
  | NextSong a
  | Init a
  | AddToPlaylist Song a
  | RemoveFromPlaylist PlaylistEntry a

type PlayerEffects e = (aws :: AWS, console :: CONSOLE, audio :: AUDIO | e)

player :: forall e. Audio -> H.Component HH.HTML Query Unit Void (Aff (PlayerEffects e))
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
    nextToSchedule: Nothing,
    audioCache: Map.empty,
    idGenState: 0
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

  renderPlaylistEntry :: forall a. (PlaylistEntry -> Unit -> Query Unit) -> PlaylistEntry -> H.ComponentHTML Query
  renderPlaylistEntry clickHandler entry =
    let Song { title, album, artist } = entry.song
    in
      HH.div
        [ HP.class_ (H.ClassName "song-list__song")
        , HE.onClick (HE.input_ (clickHandler entry))
        ]
        [ HH.div [ HP.class_ (H.ClassName "song-list__song-title") ] [ HH.text title ]
        , HH.div [ HP.class_ (H.ClassName "song-list__song-album") ] [ HH.text album ]
        , HH.div [ HP.class_ (H.ClassName "song-list__song-artist") ] [ HH.text artist ]
        ]

  renderPlaylist :: Playlist -> H.ComponentHTML Query
  renderPlaylist playlist =
    HH.div [ HP.class_ (H.ClassName "song-list") ]
      (map (renderPlaylistEntry RemoveFromPlaylist) playlist)

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

  genNextEntryId :: H.ComponentDSL State Query Void (Aff (PlayerEffects e)) Int
  genNextEntryId = do
    prevId <- H.gets _.idGenState
    let nextId = prevId + 1
    H.modify (\s -> s { idGenState = nextId })
    pure nextId

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (PlayerEffects e))
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
      playlist <- H.gets _.playlist
      nextSegmentIdx <- nextSongSegment
      H.liftEff (dropScheduled audio)
      H.modify (\s -> s { nextToSchedule = nextSegmentIdx })
      pure next
    Init next -> do
      _ <- H.fork fillQueue
      songs <- H.liftAff getSongs
      H.modify (\s -> s { songs = songs })
      pure next
    AddToPlaylist song next -> do
      playlist <- H.gets _.playlist
      entryId <- genNextEntryId
      when (length playlist == 0) $ do
        H.modify (\s -> s { nextToSchedule = Just (PlaylistSegmentIdx { entryId: entryId, segmentWithinEntryIdx: 1 }) })
      H.modify (\s -> s { playlist = playlist <> [{ song: song, entryId: entryId }] })
      pure next
    RemoveFromPlaylist entry next -> do
      -- TODO: If currently playing song is removed, the next song should start playing
      H.modify (\s -> s { playlist = filter (\e -> e.entryId /= entry.entryId) s.playlist})
      pure next

  nextSongSegment :: H.ComponentDSL State Query Void (Aff (PlayerEffects e)) (Maybe PlaylistSegmentIdx)
  nextSongSegment = do
    playlist <- H.gets _.playlist
    info <- H.liftEff (playbackInfo audio)
    let playingSegmentMaybe =
          case decodeJson info of
            Right segmentIdx -> segmentIdx
            _ -> Nothing
    let nextSegmentIdx = do
          PlaylistSegmentIdx playingSegment <- playingSegmentMaybe
          nextSongFirstSegment playlist playingSegment.entryId
    pure nextSegmentIdx

  getSegmentAudio :: SongSegmentIdx -> H.ComponentDSL State Query Void (Aff (PlayerEffects e)) AudioBuffer
  getSegmentAudio segmentIdx@(SongSegmentIdx { songUuid, segmentWithinSongIdx } ) = do
    cache <- H.gets _.audioCache
    case Map.lookup segmentIdx cache of
      Just segmentAudio -> pure segmentAudio
      Nothing -> do
        buf <- H.liftAff (getArrayBufferObject (songUuid <> "-" <> show segmentWithinSongIdx <> ".ogg"))
        audioData <- H.liftEff (decode audio buf)
        H.modify (\s -> s { audioCache = Map.insert segmentIdx audioData s.audioCache })
        pure audioData

  fillQueue :: H.ComponentDSL State Query Void (Aff (PlayerEffects e)) Unit
  fillQueue = do
    playlist <- H.gets _.playlist
    len <- H.liftEff (queueLength audio)

    -- TODO: Move to separate definition
    nextSongFirstSegmentMaybe <- nextSongSegment
    case nextSongFirstSegmentMaybe >>= songSegmentIdx playlist of
      Just nextSongFirstSegment -> do
         _ <- getSegmentAudio nextSongFirstSegment
         pure unit
      Nothing -> pure unit

    -- TODO: Move to separate definition
    when (len < 5) $ do
      nextToScheduleMaybe <- H.gets _.nextToSchedule
      case nextToScheduleMaybe of
        Just nextToSchedule ->
          case songSegmentIdx playlist nextToSchedule of
            Just nextToScheduleSongSegmentIdx -> do
              audioData <- getSegmentAudio nextToScheduleSongSegmentIdx
              H.liftEff (enqueue audio audioData (encodeJson nextToSchedule))
              H.modify (\s -> s { nextToSchedule = nextSegment playlist nextToSchedule })
            Nothing -> pure unit
        Nothing -> pure unit
    H.liftAff (delay (Milliseconds 200.0))
    fillQueue

songSegmentIdx :: Playlist -> PlaylistSegmentIdx -> Maybe SongSegmentIdx
songSegmentIdx playlist (PlaylistSegmentIdx { entryId, segmentWithinEntryIdx }) = do
  {song: Song { uuid: songUuid }} <- findEntry playlist entryId
  Just (SongSegmentIdx { songUuid: songUuid, segmentWithinSongIdx: segmentWithinEntryIdx })

nextSegment :: Playlist -> PlaylistSegmentIdx -> Maybe PlaylistSegmentIdx
nextSegment playlist (PlaylistSegmentIdx { entryId, segmentWithinEntryIdx }) = do
  playlistEntry <- find (\e -> e.entryId == entryId) playlist
  let Song song = playlistEntry.song
  if segmentWithinEntryIdx >= song.numSegments
    then
      nextSongFirstSegment playlist playlistEntry.entryId
    else
      pure (PlaylistSegmentIdx { entryId: entryId, segmentWithinEntryIdx: segmentWithinEntryIdx + 1 })

nextSongEntryId :: Playlist -> EntryId -> Maybe EntryId
nextSongEntryId playlist entryId =
  case take 2 (dropWhile (\e -> e.entryId /= entryId) playlist) of
    [current, next] -> Just next.entryId
    _ -> Nothing

nextSongFirstSegment :: Playlist -> EntryId -> Maybe PlaylistSegmentIdx
nextSongFirstSegment playlist entryId = do
  nextSong <- nextSongEntryId playlist entryId
  pure (PlaylistSegmentIdx { entryId: nextSong, segmentWithinEntryIdx: 1 })

findEntry :: Playlist -> EntryId -> Maybe PlaylistEntry
findEntry playlist playlistEntryId = find (\e -> e.entryId == playlistEntryId) playlist

main :: Eff (PlayerEffects (HA.HalogenEffects ())) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  audio <- H.liftEff initAudio
  runUI (player audio) unit body
