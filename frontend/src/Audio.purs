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

decode :: forall e. Audio -> ArrayBuffer -> Eff (audio :: AUDIO | e) AudioBuffer
decode = _decode

enqueue :: forall e. Audio -> AudioBuffer -> Eff (audio :: AUDIO | e) Unit
enqueue = _enqueue

startPlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit
startPlayback = _startPlayback

pausePlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit
pausePlayback = _pausePlayback

foreign import queueLength :: forall e. Audio -> Eff (audio :: AUDIO | e) Int

foreign import _initAudio :: forall e. Eff (audio :: AUDIO | e) Audio
foreign import _decode :: forall e. Audio -> ArrayBuffer -> Eff (audio :: AUDIO | e) AudioBuffer
foreign import _enqueue :: forall e. Audio -> AudioBuffer -> Eff (audio :: AUDIO | e) Unit
foreign import _startPlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit
foreign import _pausePlayback :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit
