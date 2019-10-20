module Main where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (and, intercalate, or)
import Data.Int (fromString) as INT
import Data.List (List(..), elemIndex, filter, fromFoldable, group, length, sort, (!!), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, lookup, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Readline (ReadlineInterface, closeInterface, createInterface, question) as RL

data Player
  = X
  | O

newtype Token = Token (Maybe Player)

type Position = Int

type Board = List (Tuple Position Token)

data GameOver
  = Winner Player
  | Draw

type WinSeq
  = List (Array Int)

type State
  = { active :: Player
    , board :: Board
    }

instance showToken :: Show Token where
  show (Token (Just p)) = show p
  show (Token Nothing) = "_"

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

instance showGameOver :: Show GameOver where
  show (Winner x) = "Winner is " <> show x <> " !!"
  show Draw = "It's a Draw!"

showBoard :: Board -> String
showBoard Nil = ""
showBoard (t : ts) = showTurn t <> showBoard ts
  where
  showTurn (Tuple p tok)
    | p `mod` 3 == 0 = show tok <> "\n"
    | otherwise = show tok <> " "

logBoard :: Board -> Aff Unit
logBoard = liftEffect <<< log <<< showBoard

allMatch :: ∀ a. Eq a => List a -> Boolean
allMatch (a : b : xs) = a == b && allMatch (b : xs)
allMatch _ = true

isPlayer :: Token -> Boolean
isPlayer (Token (Just _)) = true
isPlayer (Token Nothing) = false

chkWin :: WinSeq -> Board -> Boolean
chkWin w b = or $ map (allMatch <<< map (flip lookup b) <<< fromFoldable) w

chkDraw :: WinSeq -> Board -> Boolean
chkDraw w b = (_ == length w) $ length $ filter (drawLogic <<< map (flip lookup b) <<< fromFoldable) w
  where
  -- Draw Logic:
  -- 1. A combination ie [_,_,_] should have atleast 2 player turns AND
  -- 2. The middle element of a combination should be a player turn (X or O) ie. 
  --    It shouldn't be an empty turn. (_)
  drawLogic a = 
    or $ map and
      [ [ (_ >= 2) $ distinctCount $ filter isPlayer' a 
        , isPlayer' $ join $ a !! 1
        ]
      , [ (_ >= 2) $ distinctCount $ filter isPlayer' a 
        , not $ isPlayer' $ join $ a !! 1
        ]
      ]
    
  distinctCount = (_ - 1) <<< length <<< group <<< sort

  isPlayer' :: Maybe Token -> Boolean
  isPlayer' mt = fromMaybe false (isPlayer <$> mt) 

isGameOver :: State -> Maybe GameOver
isGameOver { active, board } =
  let
    winSeq = fromFoldable
      [ [ 7,8,9 ]
      , [ 4,5,6 ]
      , [ 1,2,3 ]
      , [ 7,4,1 ]
      , [ 8,5,2 ]
      , [ 9,6,3 ]
      , [ 7,5,3 ]
      , [ 1,5,9 ]
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

validateTurn :: Board -> String -> Either String Position
validateTurn b =
  isEmpty >>> note "Turn can't be empty." 
  >=> INT.fromString >>> note "Couldn't convert turn to Int." 
  >=> isValidTurn >>> note "Turn is invalid."
  >=> isAlreadyPlayed b >>> note "Turn already played."
  where
  isEmpty :: String -> Maybe String
  isEmpty t = if t == "" then Nothing else Just t

  toInt :: String -> Maybe Position
  toInt = INT.fromString 

  isValidTurn :: Position -> Maybe Position
  isValidTurn p = p <$ (elemIndex p (1:2:3:4:5:6:7:8:9:Nil))

  isAlreadyPlayed :: Board -> Position -> Maybe Position
  isAlreadyPlayed board p = p <$ (elemIndex p $ map fst $ filter (not <<< isPlayer <<< snd) board)

getInputPos :: RL.ReadlineInterface -> State -> Aff Position
getInputPos rl s = do
  t <- RL.question rl (question s)
  liftEffect $ log "\n"
  case validateTurn s.board t of
    Right p -> pure p
    Left  e -> do
      liftEffect $ log (e <> "\n")
      getInputPos rl s
  where
  question :: State -> String
  question s = 
    "Choose a postion for " <> show s.active <> ": " <> (availablePos s.board) <> "\n"
    where
    availablePos = intercalate "," <<< map (show <<< fst) <<< filter (not <<< isPlayer <<< snd)

playTurn :: State -> Position -> State
playTurn s p = 
  s { board =  map (\t -> iff (Token (Just s.active) <$ t) t $ (_ == p) $ fst t) s.board }
  where
  iff th el r 
    | r = th
    | otherwise = el   

togglePlayer :: State -> State
togglePlayer s = s { active = toggle s.active } 
  where
  toggle :: Player -> Player
  toggle X = O
  toggle O = X

runGame :: RL.ReadlineInterface -> State -> Aff GameOver
runGame rl s = do
  pos <- getInputPos rl s
  updated <- pure $ playTurn s pos
  logBoard updated.board
  case isGameOver updated of
    Just p  -> pure p
    Nothing -> runGame rl (togglePlayer updated)

initial :: State
initial =
  { active: O
  , board: (\i -> Tuple i (Token Nothing)) <$> fromFoldable [7,8,9,4,5,6,1,2,3]
  }

main :: Effect Unit
main = launchAff_ do
  rl <- RL.createInterface
  logBoard initial.board
  res <- runGame rl initial
  logShow res
  RL.closeInterface rl