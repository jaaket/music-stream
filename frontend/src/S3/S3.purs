module S3 where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)

foreign import data AWS :: Effect

getJsonObject :: forall e. String -> Aff (aws :: AWS | e) Json
getJsonObject = fromEffFnAff <<< _getJsonObject

getArrayBufferObject :: forall e. String -> Aff (aws :: AWS | e) ArrayBuffer
getArrayBufferObject = fromEffFnAff <<< _getArrayBufferObject

foreign import _getJsonObject :: forall eff. String -> EffFnAff (aws :: AWS | eff) Json
foreign import _getArrayBufferObject :: forall eff. String -> EffFnAff (aws :: AWS | eff) ArrayBuffer
