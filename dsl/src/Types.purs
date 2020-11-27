module Types where

import Prelude

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map (lookup) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

data Player
   = X
   | O

type Position = Int

newtype Cell
   = Cell (Maybe Player)

newtype Board
   = Board (Map Position Cell)

type GameState
   = { active :: Player
     , board  :: Board
     }

data Result
  = Winner Player
  | Draw

data ValError
   = EmptyPosition
   | CantConvertToNumber
   | InvalidPosition
   | PositionAlreadyPlayed

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

instance eqCell :: Eq Cell where
   eq (Cell (Just x)) (Cell (Just o)) = x == o
   eq (Cell Nothing ) (Cell Nothing ) = false
   eq (Cell _       ) (Cell _       ) = false

instance ordCell :: Ord Cell where
   compare (Cell x) (Cell o) = compare x o

instance showCell :: Show Cell where
   show (Cell (Just p)) = show p
   show (Cell Nothing ) = " "

instance showResult :: Show Result where
   show (Winner x) = "Winner is " <> show x <> " !!"
   show Draw       = "It's a Draw!"

instance showValError :: Show ValError where
   show EmptyPosition         = "Position can't be empty." 
   show CantConvertToNumber   = "Couldn't convert turn to Int." 
   show InvalidPosition       = "Position is invalid."
   show PositionAlreadyPlayed = "Position already played."

instance showBoard :: Show Board where
   show :: Board -> String
   show board =
      foldMap showMove [7,8,9,4,5,6,1,2,3]
      where
         showMove p
            | p == 3         = " " <> show (getCell board p) <> "\n"
            | p `mod` 3 == 0 = " " <> show (getCell board p) <> "\n---|---|---\n"
            | otherwise      = " " <> show (getCell board p) <> " |"
            
derive instance newtypeBoard :: Newtype Board _
derive instance newtypeCell :: Newtype Cell _

instance semigroupCell :: Semigroup Cell where
  append _ _ = Cell Nothing

instance monoidCell :: Monoid Cell where
  mempty = Cell Nothing

getCell :: Board -> Position -> Cell
getCell (Board b) pos
   = case (M.lookup pos b) of
      Just cell -> cell
      Nothing   -> mempty