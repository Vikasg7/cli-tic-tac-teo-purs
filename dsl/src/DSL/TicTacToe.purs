module TicTacToe where

import Prelude

import Types (Position, Result(..), Seconds, ValError, GameState)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Helper (updateActive, updateCell)
import Logger (LOGGER, ask, erase, print, printShow, runLogger)
import Logic (isDraw, isWon, switchPlayer, valPos)
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), case_, on)
import Run (interpret, lift) as Run
import Run.State (STATE, evalState, get, modify)
import Sleep (SLEEP, runSleep, sleep)

data TicTacToeF a
   = ShowBoard a
   | AskPosition (String -> a)
   | ValidatePosition String ((Either ValError Position) -> a)
   | ShowErrorFor Seconds ValError a
   | PlayPosition Position a
   | IsGameOver (Maybe Result -> a)
   | ShowResult Result a
   | TogglePlayer a

derive instance functorTicTacToe :: Functor TicTacToeF

type TICTACTOE = FProxy TicTacToeF

_tictactoe = SProxy :: SProxy "tictactoe"

-- Combinators
showBoard :: ∀ r. Run (tictactoe :: TICTACTOE | r) Unit
showBoard = Run.lift _tictactoe (ShowBoard unit)

askPosition :: ∀ r. Run (tictactoe :: TICTACTOE | r) String
askPosition = Run.lift _tictactoe (AskPosition identity)

validatePosition :: ∀ r. String -> Run (tictactoe :: TICTACTOE | r) (Either ValError Position)
validatePosition str = Run.lift _tictactoe (ValidatePosition str identity)

showErrorFor :: ∀ r. Seconds -> ValError -> Run (tictactoe :: TICTACTOE | r) Unit
showErrorFor seconds valErr = Run.lift _tictactoe (ShowErrorFor seconds valErr unit)

playPosition :: ∀ r. Position -> Run (tictactoe :: TICTACTOE | r) Unit
playPosition pos = Run.lift _tictactoe (PlayPosition pos unit)

isGameOver :: ∀ r. Run (tictactoe :: TICTACTOE | r) (Maybe Result)
isGameOver = Run.lift _tictactoe (IsGameOver identity)

showResult :: ∀ r. Result -> Run (tictactoe :: TICTACTOE | r) Unit
showResult res = Run.lift _tictactoe (ShowResult res unit)

togglePlayer :: ∀ r. Run (tictactoe :: TICTACTOE | r) Unit
togglePlayer = Run.lift _tictactoe (TogglePlayer unit)

-- Interpretor
handleTicTacToe :: ∀ a r. TicTacToeF a -> Run ( state :: STATE GameState, logger :: LOGGER, sleep :: SLEEP | r ) a
handleTicTacToe = case _ of
   ShowBoard a -> do
      board <- getBoard
      erase -- Clearing previous board
      printShow board
      pure a

   AskPosition reply -> do
      active <- getActive
      m <- ask $ "Enter Position for " <> (show active) <> ".\n"
      pure $ reply m

   ValidatePosition s reply -> do
      board <- getBoard
      pure $ reply (valPos board s)
   
   ShowErrorFor s e a -> do
      print (show e <> "\n")
      sleep s
      pure a
      
   PlayPosition pos a -> do
      active <- getActive
      modify (updateCell pos active)
      pure a

   ShowResult r a -> do
      printShow r
      pure a
   
   TogglePlayer a -> do
      modify (updateActive switchPlayer)
      pure a

   IsGameOver reply -> do
      { active, board } <- getState 
      pure $ reply $ 
         if isWon board
            then Just (Winner active)
            else if isDraw board
               then Just Draw
               else Nothing

   where
      getState = get

      getBoard = (_.board) <$> getState

      getActive = (_.active) <$> getState

runTicTacToe
   :: ∀ a r
    . GameState
   -> Run (tictactoe :: TICTACTOE) a
   -> Run (aff :: AFF, effect :: EFFECT | r) a
runTicTacToe initialState program =
   Run.interpret (case_ # on _tictactoe handleTicTacToe) program
   # evalState initialState
   # runLogger
   # runSleep