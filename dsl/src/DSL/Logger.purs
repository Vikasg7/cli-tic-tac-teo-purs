module Logger where

import Prelude

import Effect.Class.Console (clear, log)
import Prompt (prompt)
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), liftAff, on, send)
import Run (interpret, lift) as Run

data LoggerF a 
   = Print String a
   | Ask String (String -> a)
   | Erase a

derive instance functorLogger :: Functor LoggerF

type LOGGER = FProxy LoggerF

_logger = SProxy :: SProxy "logger"

print :: ∀ r. String -> Run (logger :: LOGGER | r) Unit
print s = Run.lift _logger (Print s unit)

ask :: ∀ r. String -> Run (logger :: LOGGER | r) String
ask q = Run.lift _logger (Ask q identity)

erase :: ∀ r. Run (logger :: LOGGER | r) Unit
erase = Run.lift _logger (Erase unit)

printShow :: ∀ r a. Show a => a -> Run (logger :: LOGGER | r) Unit
printShow = print <<< show

handleLogger :: ∀ a r. LoggerF a -> Run (effect :: EFFECT, aff :: AFF | r) a
handleLogger = case _ of
   Print s a -> do
      log s
      pure a
   
   Ask q reply -> do
      m <- liftAff $ prompt q
      log "\n"
      pure (reply m) 
   
   Erase a -> do
      clear
      pure a

runLogger 
   :: ∀ a r
    . Run (effect :: EFFECT, aff :: AFF, logger :: LOGGER | r) a
   -> Run (effect :: EFFECT, aff :: AFF | r) a
runLogger = Run.interpret (send # on _logger handleLogger)