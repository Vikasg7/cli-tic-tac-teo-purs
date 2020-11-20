module Main where

import Prelude

import Types (Board(..), Cell(..), GameState, Player(..), Position)
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Run (Run, runBaseAff')
import TicTacToe (TICTACTOE, askPosition, isGameOver, playPosition, runTicTacToe, showBoard, showErrorFor, showResult, togglePlayer, validatePosition)

getPosition :: ∀ r. Run (tictactoe :: TICTACTOE | r) Position
getPosition = do
   showBoard
   res <- askPosition
   maybePos <- validatePosition res
   case maybePos of
      Right pos -> pure pos
      Left err -> do
         showErrorFor 1 err
         getPosition  

game :: ∀ r. Run (tictactoe :: TICTACTOE | r) Unit
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
      # runBaseAff'
      # launchAff_
   where
      initialState :: GameState
      initialState =
         { active: O
         , board: Board $ fromFoldable $ (\i -> Tuple i (Cell Nothing)) <$> [7,8,9,4,5,6,1,2,3]
         }