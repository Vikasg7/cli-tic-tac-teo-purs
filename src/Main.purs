module Main where

import Prelude

import Data.Either (Either, note)
import Data.List (List(..), any, filter, fromFoldable, group, length, range, sort, updateAt, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), lookup)
import Effect (Effect)
import Effect.Class.Console (logShow)

data Player
  = X
  | O

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
     
newtype Token
  = Token (Maybe Player)

instance showToken :: Show Token where
  show (Token (Just p)) = show p
  show (Token Nothing) = " "

instance eqToken :: Eq Token where
  eq (Token (Just x)) (Token (Just o)) = x == o
  eq (Token Nothing) (Token Nothing) = false
  eq (Token _) (Token _) = false

instance ordToken :: Ord Token where
  compare (Token x) (Token o) = compare x o

type Position = Int

type Board
  = List (Tuple Position Token)

data GameOver
  = Winner Player
  | Draw

instance showGameOver :: Show GameOver where
  show (Winner x) = "Winner is " <> show x <> " !!"
  show Draw = "It's a Draw!"

type WinSeq
  = List (Array Int)

type State
  = { active :: Player
    , board :: Board
    }

showBoard :: Board -> String
showBoard Nil = "Board can't be empty."
showBoard (t : ts) = showTurn t <> showBoard ts
  where
  showTurn (Tuple p tok)
    | p `mod` 3 == 0 = show tok <> "\n"
    | otherwise = show tok <> " "

allMatch :: ∀ a. Eq a => List a -> Boolean
allMatch (a : b : xs) = a == b && allMatch (b : xs)
allMatch _ = true

isGameOver :: State -> Maybe GameOver
isGameOver { active, board } =
  let
    winSeq = fromFoldable
      [ [ 1, 2, 3 ]
      , [ 4, 5, 6 ]
      , [ 7, 8, 9 ]
      , [ 1, 4, 7 ]
      , [ 2, 5, 8 ]
      , [ 3, 6, 9 ]
      , [ 1, 5, 9 ]
      , [ 3, 5, 7 ]
      ]

    isGameWon = chkWin winSeq board

    isDraw = chkDraw winSeq board
  in
    if isGameWon then
      Just (Winner active)
    else
      if isDraw then
        Just Draw
      else
        Nothing

chkWin :: WinSeq -> Board -> Boolean
chkWin w b = any identity $ map (allMatch <<< map (flip lookup b) <<< fromFoldable) w

chkDraw :: WinSeq -> Board -> Boolean
chkDraw w b = (_ == length w) $ length $ filter ((_ >= 2) <<< distinctCount <<< filter (isPlayer <<< flip lookup b) <<< fromFoldable) w
  where
  distinctCount = length <<< group <<< sort

  isPlayer :: Maybe Token -> Boolean
  isPlayer (Just (Token (Just _))) = true
  isPlayer (Just (Token Nothing))  = false
  isPlayer Nothing                 = false 

playTurn :: State -> Position -> Either String State
playTurn s p = do
  board <- note "Game State update failed!" $ updateAt p (Tuple p (Token (Just s.active))) s.board
  pure
    { active : togglePlayer s.active
    , board
    }

togglePlayer :: Player -> Player
togglePlayer X = O
togglePlayer O = X

initial :: State
initial =
  { active: O
  , board: (\i -> Tuple i (Token Nothing)) <$> range 1 9
  }

main :: Effect Unit
main = do
  logShow $ isGameOver initial
