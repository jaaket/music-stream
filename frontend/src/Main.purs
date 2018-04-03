module Main where

import Audio
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
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

player :: forall e. Audio -> AudioBuffer -> H.Component HH.HTML Query Unit Void (Aff (audio :: AUDIO | e)) 
player audio audioData =
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
      if nextPlaying
        then H.liftEff (schedule audio audioData 0.0)
        else H.liftEff (stopPlayback audio)
      H.modify (\s -> s { playing = nextPlaying })
      pure next
    IsPlaying reply -> do
      playing <- H.gets _.playing
      pure (reply playing)


main :: Eff (ajax :: AJAX, audio :: AUDIO, console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  arrayBuffer1 <- H.liftAff (get "http://192.168.2.33:8080/get/out000.ogg")
  arrayBuffer2 <- H.liftAff (get "http://192.168.2.33:8080/get/out001.ogg")
  arrayBuffer3 <- H.liftAff (get "http://192.168.2.33:8080/get/out002.ogg")
  audio <- H.liftEff initAudio
  audioData1 <- H.liftAff (decode audio arrayBuffer1.response)
  audioData2 <- H.liftAff (decode audio arrayBuffer2.response)
  audioData3 <- H.liftAff (decode audio arrayBuffer3.response)
  H.liftEff (log (show (bufferLength audioData1)))
  H.liftEff (log (show (bufferLength audioData2)))
  H.liftEff (log (show (duration audioData1)))
  H.liftEff (log (show (duration audioData2)))
  H.liftEff (schedule audio audioData1 2.0)
  H.liftEff (schedule audio audioData2 (2.0 + duration audioData1))
  H.liftEff (schedule audio audioData3 (2.0 + duration audioData1 + duration audioData2))
  runUI (player audio audioData1) unit body
