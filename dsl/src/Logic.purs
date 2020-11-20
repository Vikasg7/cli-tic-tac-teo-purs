module Logic where

import Prelude

import Types (Board, Cell(..), Player(..), Position, ValError(..))
import Data.Either (Either, note)
import Data.Foldable (or)
import Data.Int (fromString) as INT
import Data.List (List, elemIndex, filter, fromFoldable, group, length, range, sort, (:))
import Data.Maybe (Maybe(..))
import Helper (getCell, getPlayer)

valPos :: Board -> String -> Either ValError Position
valPos b = 
   isEmpty                 >>> note EmptyPosition 
   >=> INT.fromString      >>> note CantConvertToNumber 
   >=> isValidTurn         >>> note InvalidPosition
   >=> isAlreadyPlayed b   >>> note PositionAlreadyPlayed
   where
      isEmpty :: String -> Maybe String
      isEmpty t = if t == "" then Nothing else Just t

      isValidTurn :: Position -> Maybe Position
      isValidTurn pos = pos <$ (elemIndex pos (range 1 9))

      isAlreadyPlayed :: Board -> Position -> Maybe Position
      isAlreadyPlayed board pos = 
         case getPlayer pos board of
            Just _  -> Nothing
            Nothing -> Just pos

winSeq :: List (List Int)
winSeq = fromFoldable $ fromFoldable <$> [ [ 7,8,9 ], [ 7,4,1 ]
                                         , [ 4,5,6 ], [ 8,5,2 ]
                                         , [ 1,2,3 ], [ 9,6,3 ]
                                         , [ 7,5,3 ], [ 1,5,9 ]
                                         ]

isWon :: Board -> Boolean
isWon board = or $ (allMatch <<< map (getCell board)) <$> winSeq
   where
      allMatch :: ∀ c. Eq c => List c -> Boolean
      allMatch (x : y : cs) = (x == y) && allMatch (y : cs)
      allMatch _            = true

isDraw :: Board -> Boolean
isDraw board = 
   (_ == length winSeq) $ length $ filter (atleastTwoPlayerTurns <<< map (getCell board)) winSeq
   where
      atleastTwoPlayerTurns a = (_ >= 2) $ distinctCount $ filter isPlayer a
      distinctCount = length <<< group <<< sort

isPlayer :: Cell -> Boolean
isPlayer (Cell (Just _)) = true
isPlayer _               = false

switchPlayer :: Player -> Player
switchPlayer = case _ of
   X -> O
   O -> X