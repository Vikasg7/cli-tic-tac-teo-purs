module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Readline (closeInterface, createInterface, prompt) as RL

main :: Effect Unit
main = launchAff_ $ do
   rl <- RL.createInterface
   a  <- RL.prompt rl "What is your name?\n"
   liftEffect $ log a 
   RL.closeInterface rl