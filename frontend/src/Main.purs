module Main where

import Audio
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (zip, cons, (..))
import Data.Maybe (Maybe(..))
import Data.Traversable (scanl, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (get, AJAX)


type State = {
  playing :: Boolean
}

data Query a
  = Toggle a
  | IsPlaying (Boolean -> a)

player :: forall e. Audio -> H.Component HH.HTML Query Unit Void (Aff (audio :: AUDIO | e))
player audio =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {
    playing: false
  }

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state.playing then "⏸" else "▶"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (audio :: AUDIO | e))
  eval = case _ of
    Toggle next -> do
      playing <- H.gets _.playing
      let nextPlaying = not playing
      -- if nextPlaying
      --   then H.liftEff (schedule audio audioData 0.0)
      --   else H.liftEff (stopPlayback audio)
      H.modify (\s -> s { playing = nextPlaying })
      pure next
    IsPlaying reply -> do
      playing <- H.gets _.playing
      pure (reply playing)

main :: Eff (ajax :: AJAX, audio :: AUDIO, console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let baseUri = "http://localhost:8099"
  audio <- H.liftEff initAudio
  let files = map ((_ <> ".ogg") <<< ("/get/out" <> _) <<< show) (1..5)
  arrayBuffers <- traverse ((baseUri <> _) >>> get >>> H.liftAff) files
  H.liftEff (log "ASD")
  audioDatas <- traverse (_.response >>> decode audio >>> H.liftEff) arrayBuffers
  H.liftEff (log "QWE")
  let spliceOffsets = cons 2.0 (scanl (+) 2.0 (map duration audioDatas))
  H.liftEff (log (show spliceOffsets))
  traverse_ ((\(Tuple d t) -> schedule audio d t) >>> H.liftEff) (zip audioDatas spliceOffsets)
  runUI (player audio) unit body
