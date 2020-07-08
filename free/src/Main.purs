module Main where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (and, intercalate, or)
import Data.Functor (mapFlipped)
import Data.Int (fromString) as INT
import Data.List (List(..), elemIndex, filter, fromFoldable, range, sort, (!!), (:), group, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, lookup, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Effect.Class (liftEffect) as EFF
import Effect.Console (log)
import Readline (ReadlineInterface)
import Readline as RL
import Run (AFF, EFFECT, Run, liftAff, liftEffect, runBaseAff')
import Run.Reader (READER, ask, runReader)
import Run.State (STATE, evalState, get, modify)
import Types (Board, Game, GameState, Player(..), Position, Result(..), Token(..), WinSeq, Config)

main :: Effect Unit
main = launchAff_ do
   rl <- RL.createInterface
   logBoard' initial.board
   res <- runGame rl winSeq initial game
   logShow res
   RL.closeInterface rl
   where
      logBoard' = EFF.liftEffect <<< logBoard

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
         logBoard' updated.board
         result <- isGameOver
         case result of
            Just a  -> pure a
            Nothing -> togglePlayer *> game
         where
            logBoard' = liftEffect <<< logBoard
         
            togglePlayer :: ∀ r. Run (state :: STATE GameState | r) Unit
            togglePlayer = 
               modify (\s -> s { active = toggle s.active })
               where
                  toggle :: Player -> Player
                  toggle X = O
                  toggle O = X

runGame :: ReadlineInterface -> WinSeq -> GameState -> Game Result -> Aff Result
runGame rl winSeq gameState game
   = game 
      # runReader ({rl, winSeq})
      # evalState gameState
      # runBaseAff'

getInputPos 
   :: ∀ r.
      Run ( reader :: READER Config
          , state  :: STATE GameState
          , effect :: EFFECT
          , aff    :: AFF
          | r
          )
          Position
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
      log' = liftEffect <<< log

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

playTurn :: ∀ r. Position -> Run (state :: STATE GameState | r) GameState 
playTurn position = do
   modify (update position)
   s <- getState
   pure s
   where
      getState = get

      update p s 
         = s { board = mapFlipped s.board \t -> 
               (if _ then Token (Just s.active) <$ t else t) $ (_ == p) $ fst t
            }

isGameOver 
   :: ∀ r. 
      Run ( reader :: READER Config
          , state  :: STATE GameState
          | r
          )
          (Maybe Result)
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

logBoard :: Board -> Effect Unit
logBoard 
   = log <<< showBoard
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
