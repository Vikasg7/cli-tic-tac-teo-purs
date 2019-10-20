module Main where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (and, or)
import Data.Int (fromString) as INT
import Data.List (List(..), elemIndex, filter, fromFoldable, group, length, range, sort, updateAt, (!!), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, lookup)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Readline (ReadlineInterface, closeInterface, createInterface, prompt) as RL

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
  show (Token Nothing) = "_"

instance eqToken :: Eq Token where
  eq (Token (Just x)) (Token (Just o)) = x == o
  eq (Token Nothing) (Token Nothing) = false
  eq (Token _) (Token _) = false

instance ordToken :: Ord Token where
  compare (Token x) (Token o) = compare x o

type Position
  = Int

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
showBoard Nil = ""
showBoard (t : ts) = showTurn t <> showBoard ts
  where
  showTurn (Tuple p tok)
    | p `mod` 3 == 0 = show tok <> "\n"
    | otherwise = show tok <> " "

showBoardAff :: Board -> Aff Unit
showBoardAff = liftEffect <<< log <<< showBoard

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
  where
  chkWin :: WinSeq -> Board -> Boolean
  chkWin w b = or $ map (allMatch <<< map (flip lookup b) <<< fromFoldable) w

  chkDraw :: WinSeq -> Board -> Boolean
  chkDraw w b = (_ == length w) $ length $ filter (drawLogic <<< map (flip lookup b) <<< fromFoldable) w
    where
    -- Draw Logic:
    -- 1. A combination ie [_,_,_] should have atleast 2 player turns AND
    -- 2. The middle element of a combination should be a player turn (X or O) ie. 
    --    It shouldn't be an empty turn. (_)
    drawLogic a = and
      [ (_ >= 2) $ distinctCount $ map isPlayer a 
      , isPlayer $ join $ a !! 1
      ]
      
    distinctCount = (_ - 1) <<< length <<< group <<< sort

    isPlayer :: Maybe Token -> Boolean
    isPlayer (Just (Token (Just _))) = true
    isPlayer (Just (Token Nothing))  = false
    isPlayer Nothing = false

playTurn :: State -> Position -> Either String State
playTurn s p = do
  b <- note "Game State update failed!" $ updateAt (p-1) (Tuple p (Token (Just s.active))) s.board
  pure $ s { board = b }

togglePlayer :: State -> State
togglePlayer s = s { active = toggle s.active } 
  where
  toggle :: Player -> Player
  toggle X = O
  toggle O = X

runGame :: RL.ReadlineInterface -> State -> Aff (Either String GameOver)
runGame rl s = do
  turn <- RL.prompt rl $ availableTurns s
  boardUpdated <- pure $ playTurn s =<< (note ("Couldn't convert " <> turn <> " to Int") $ INT.fromString turn)
  case boardUpdated of
    Right stateWithUpdatedBoard -> do
      showBoardAff stateWithUpdatedBoard.board
      case isGameOver stateWithUpdatedBoard of
        Just p  -> pure $ Right p
        Nothing -> runGame rl (togglePlayer stateWithUpdatedBoard)
    Left e -> pure $ Left e
  where
  availableTurns :: State -> String
  availableTurns { active, board } = "Waiting for next turn.\n"

validateTurn :: Board -> String -> Either String Position
validateTurn b =
  isEmpty               >>> note "Turn can't be empty." 
  >=> INT.fromString    >>> note "Couldn't convert turn to Int." 
  >=> isValidTurn       >>> note "Turn is invalid."
  >=> isAlreadyPlayed b >>> note "Turn already played."
  where
  isEmpty :: String -> Maybe String
  isEmpty t = if t == "" then Nothing else Just t

  toInt :: String -> Maybe Position
  toInt = INT.fromString 

  isAlreadyPlayed :: Board -> Position -> Maybe Position
  isAlreadyPlayed board p = p <$ (elemIndex p $ fst <$> board)

  isValidTurn :: Position -> Maybe Position
  isValidTurn p = p <$ (elemIndex p (1:2:3:4:5:6:7:8:9:Nil))

initial :: State
initial =
  { active: O
  , board: (\i -> Tuple i (Token Nothing)) <$> range 1 9
  }

main :: Effect Unit
main = launchAff_ do
  rl <- RL.createInterface
  showBoardAff initial.board
  res <- runGame rl initial
  logShow res
  RL.closeInterface rl