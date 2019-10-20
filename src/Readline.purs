module Readline
 ( question
 , createInterface
 , closeInterface
 , ReadlineInterface
 ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)

type Callback a = a -> Effect Unit

foreign import data ReadlineInterface :: Type

foreign import createInterface_ :: Effect ReadlineInterface

foreign import question_ :: ReadlineInterface -> String -> Callback String -> Effect Unit

foreign import closeInterface_ :: ReadlineInterface -> Effect Unit

createInterface :: Aff ReadlineInterface
createInterface = liftEffect createInterface_

closeInterface :: ReadlineInterface ->  Aff Unit
closeInterface = liftEffect <<< closeInterface_

question :: ReadlineInterface -> String -> Aff String
question rl q = makeAff \cb -> 
  question_ rl q (cb <<< Right) $> nonCanceler