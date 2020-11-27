module Sleep where

import Prelude

import Data.Time.Duration (Seconds, fromDuration)
import Effect.Aff (delay)
import Run (AFF, FProxy, Run, SProxy(..), on, send)
import Run (interpret, lift, liftAff) as Run

data SleepF a
   = Sleep Seconds a

derive instance functorSleep :: Functor SleepF

type SLEEP = FProxy SleepF

_sleep = SProxy :: SProxy "sleep"

sleep :: ∀ r. Seconds -> Run (sleep :: SLEEP | r) Unit
sleep i = Run.lift _sleep (Sleep i unit)

handleSleep :: ∀ a r. SleepF a -> Run (aff :: AFF | r) a
handleSleep = case _ of
   Sleep s a -> do
      waitFor s
      pure a
      where
         waitFor sec = Run.liftAff $ delay (fromDuration sec)

runSleep 
   :: ∀ a r
    . Run (aff :: AFF, sleep :: SLEEP | r) a
   -> Run (aff :: AFF | r) a
runSleep = Run.interpret (send # on _sleep handleSleep)