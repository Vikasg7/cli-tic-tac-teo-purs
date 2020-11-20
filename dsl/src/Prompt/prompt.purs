module Prompt where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Unsafe (unsafePerformEffect)

foreign import prompt_ :: String -> ((String -> Unit) -> Effect Unit)

prompt :: String -> Aff String
prompt q = makeAff \cb ->
   prompt_ q (toJS cb) $> nonCanceler
   where
      toJS cb = Right >>> cb >>> unsafePerformEffect