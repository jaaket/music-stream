module Audio where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Prelude (Unit)

foreign import data AUDIO :: Effect

foreign import data Audio :: Type
foreign import data AudioBuffer :: Type

initAudio :: forall e. Eff (audio :: AUDIO | e) Audio
initAudio = _initAudio

decode :: forall e. Audio -> ArrayBuffer -> Aff (audio :: AUDIO | e) AudioBuffer
decode audio arrayBuffer = fromEffFnAff (_decode audio arrayBuffer)

foreign import duration :: forall e. AudioBuffer -> Number
foreign import bufferLength :: forall e. AudioBuffer -> Number

schedule :: forall e. Audio -> AudioBuffer -> Number -> Eff (audio :: AUDIO | e) Unit
schedule = _schedule

stopPlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit
stopPlayback = _stopPlayback

foreign import _initAudio :: forall e. Eff (audio :: AUDIO | e) Audio
foreign import _decode :: forall e. Audio -> ArrayBuffer -> EffFnAff (audio :: AUDIO | e) AudioBuffer
foreign import _schedule :: forall e. Audio -> AudioBuffer -> Number -> Eff (audio :: AUDIO | e) Unit
foreign import _stopPlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit