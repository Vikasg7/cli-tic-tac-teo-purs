module Main where

import Prelude

import Data.Either (Either(..))

import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Run (Run, runBaseAff')
import Sleep (SLEEP, runSleep, sleep)
import TicTacToe (TICTACTOE, askPosition, isGameOver, playPosition, runTicTacToe, showBoard, showError, showResult, togglePlayer, validatePosition)
import Types (Board(..), Cell(..), GameState, Player(..), Position)

getPosition :: ∀ r. Run (tictactoe :: TICTACTOE, sleep :: SLEEP | r) Position
getPosition = do
   showBoard
   res <- askPosition
   maybePos <- validatePosition res
   case maybePos of
      Right pos -> pure pos
      Left err -> do
         showError err
         sleep (Seconds 1.0)
         getPosition  

game :: ∀ r. Run (tictactoe :: TICTACTOE, sleep :: SLEEP | r) Unit
game = do
   pos <- getPosition
   playPosition pos
   maybeResult <- isGameOver
   case maybeResult of
      Just result -> showBoard *> showResult result 
      Nothing     -> togglePlayer *> game

main :: Effect Unit
main = do
   runTicTacToe initialState game
      # runSleep
      # runBaseAff'
      # launchAff_
   where
      initialState :: GameState
      initialState =
         { active: O
         , board: Board $ fromFoldable $ (\i -> Tuple i (Cell Nothing)) <$> [7,8,9,4,5,6,1,2,3]
         }