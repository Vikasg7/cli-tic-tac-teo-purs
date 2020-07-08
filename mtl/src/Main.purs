module Main where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, evalStateT, get, modify_)
import Control.Monad.State.Trans (modify)
import Data.Either (Either(..), note)
import Data.Foldable (and, intercalate, or)
import Data.Functor (mapFlipped)
import Data.Int (fromString) as INT
import Data.List (List(..), elemIndex, filter, fromFoldable, range, sort, (!!), (:), group, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, lookup, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Readline (ReadlineInterface)
import Readline as RL
import Types (Board, Game, GameState, Player(..), Position, Result(..), Token(..), WinSeq, Config)

main :: Effect Unit
main = launchAff_ do
   rl <- RL.createInterface
   logBoard initial.board
   res <- runGame rl winSeq initial game
   logShow res
   RL.closeInterface rl
   where
      initial :: GameState
      initial =
         { active: O
         , board: (\i -> Tuple i (Token Nothing)) <$> fromFoldable [7,8,9,4,5,6,1,2,3]
         }
      
      winSeq :: WinSeq
      winSeq = 
         fromFoldable $ map fromFoldable
            [ [ 7,8,9 ]
            , [ 4,5,6 ]
            , [ 1,2,3 ]
            , [ 7,4,1 ]
            , [ 8,5,2 ]
            , [ 9,6,3 ]
            , [ 7,5,3 ]
            , [ 1,5,9 ]
            ]

      game :: Game Result
      game = do
         updated <- getInputPos >>= playTurn
         liftAff $ logBoard updated.board
         result <- isGameOver
         case result of
            Just a  -> pure a
            Nothing -> togglePlayer *> game
         where
            togglePlayer :: ∀ m. MonadState GameState m => m Unit
            togglePlayer = 
               modify_ (\s -> s { active = toggle s.active })
               where
                  toggle :: Player -> Player
                  toggle X = O
                  toggle O = X

runGame :: ReadlineInterface -> WinSeq -> GameState -> Game Result -> Aff Result
runGame rl winSeq gameState game
   = evalStateT (runReaderT game ({rl, winSeq})) gameState

logBoard :: Board -> Aff Unit
logBoard 
   = liftEffect <<< log <<< showBoard
   where
      showBoard :: Board -> String
      showBoard Nil = ""
      showBoard (t : ts) = 
         showTurn t <> showBoard ts
         where
            showTurn (Tuple p tok)
               | p == 3 = " " <> show tok <> "\n"
               | p `mod` 3 == 0 = " " <> show tok <> "\n-----------\n"
               | otherwise = " " <> show tok <> " |"

isPlayer :: Token -> Boolean
isPlayer (Token (Just _)) = true
isPlayer (Token Nothing) = false

getInputPos 
   :: ∀ m.
      MonadReader Config m
   => MonadState GameState m
   => MonadAff m
   => m Position
getInputPos = do
   rl <- getRL
   state <- getState
   t <- liftAff $ RL.question rl (question state)
   log' "\n"
   case validateTurn state.board t of
      Right p -> pure p
      Left  e -> do
         log' (e <> "\n")
         getInputPos
   where
      log' = liftAff <<< liftEffect <<< log

      getRL = ask >>= (_.rl >>> pure)

      getState = get

      question :: GameState -> String
      question s 
         = "Choose a postion for player " <> show s.active <> ": " <> (availablePos s.board) <> "\n"
         where
            availablePos = intercalate "," <<< map (show <<< fst) <<< filter (not <<< isPlayer <<< snd)

      validateTurn :: Board -> String -> Either String Position
      validateTurn b 
         = isEmpty >>> note "Turn can't be empty." 
         >=> INT.fromString >>> note "Couldn't convert turn to Int." 
         >=> isValidTurn >>> note "Turn is invalid."
         >=> isAlreadyPlayed b >>> note "Turn already played."
         where
            isEmpty :: String -> Maybe String
            isEmpty t = if t == "" then Nothing else Just t

            isValidTurn :: Position -> Maybe Position
            isValidTurn p = p <$ (elemIndex p (range 1 9))

            isAlreadyPlayed :: Board -> Position -> Maybe Position
            isAlreadyPlayed board p = p <$ (elemIndex p $ map fst $ filter (not <<< isPlayer <<< snd) board)

playTurn :: ∀ m. MonadState GameState m => Position -> m GameState 
playTurn position = modify (update position)
   where
      update p s 
         = s { board = mapFlipped s.board \t -> 
               (if _ then Token (Just s.active) <$ t else t) $ (_ == p) $ fst t
            }

isGameOver 
   :: ∀ m. 
      MonadState GameState m 
   => MonadReader Config m 
   => m (Maybe Result)
isGameOver = do
   winSeq             <- getWinSeq
   { active, board }  <- getState 
   let isGameWon = chkWin winSeq board
       isDraw    = chkDraw winSeq board
   pure $ if isGameWon 
            then Just (Winner active) 
            else if isDraw 
               then Just Draw 
               else Nothing
   where
      getWinSeq = ask >>= (_.winSeq >>> pure)
      getState = get

      chkWin :: WinSeq -> Board -> Boolean
      chkWin w b = or $ map (allMatch <<< map (flip lookup b) <<< fromFoldable) w
         where
            allMatch :: ∀ a. Eq a => List a -> Boolean
            allMatch (a : b : xs) = a == b && allMatch (b : xs)
            allMatch _ = true

      chkDraw :: WinSeq -> Board -> Boolean
      chkDraw w b = (_ == length w) $ length $ filter (drawLogic <<< map (flip lookup b)) w
         where
            drawLogic a =
               let
                  atleastTwoPlayerTurns = (_ >= 2) $ distinctCount $ filter isPlayer' a
                  isMiddlePlayerTurn = isPlayer' $ join $ a !! 1   
               in
                  or $ map and
                  [ [ atleastTwoPlayerTurns
                     , isMiddlePlayerTurn
                     ]
                  , [ atleastTwoPlayerTurns
                     , not $ isMiddlePlayerTurn
                     ]
                  ]
            
            distinctCount = length <<< group <<< sort

            isPlayer' :: Maybe Token -> Boolean
            isPlayer' mt = fromMaybe false (isPlayer <$> mt) 
