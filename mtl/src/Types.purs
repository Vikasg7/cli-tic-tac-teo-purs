module Types where

import Prelude

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Readline (ReadlineInterface)

type Game a = ReaderT Config (StateT GameState Aff ) a

data Player
  = X
  | O

newtype Token = Token (Maybe Player)

type Position = Int

type Board = List (Tuple Position Token)

data Result
  = Winner Player
  | Draw

type WinSeq
  = List (List Int)

type Config 
   = { winSeq :: WinSeq
     , rl :: ReadlineInterface
     }

type GameState
  = { active :: Player
    , board :: Board
    }

instance showToken :: Show Token where
  show (Token (Just p)) = show p
  show (Token Nothing) = " "

instance eqToken :: Eq Token where
  eq (Token (Just x)) (Token (Just o)) = x == o
  eq (Token Nothing) (Token Nothing) = false
  eq (Token _) (Token _) = false

instance ordToken :: Ord Token where
  compare (Token x) (Token o) = compare x o

instance eqPlayer :: Eq Player where
  eq X X = true
  eq O O = true
  eq _ _ = false

instance ordPlayer :: Ord Player where
  compare X O = GT
  compare O X = LT
  compare _ _ = EQ

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"

instance showResult :: Show Result where
  show (Winner x) = "Winner is " <> show x <> " !!"
  show Draw = "It's a Draw!"