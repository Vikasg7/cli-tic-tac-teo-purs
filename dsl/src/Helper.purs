module Helper where

import Prelude

import Types (Board, Cell, GameState, Player, Position)
import Data.Lens (Iso', Lens', _Just, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

_board :: Lens' GameState Board
_board = prop (SProxy :: SProxy "board")

_Board :: Iso' Board (Map Position Cell)
_Board = _Newtype

_Cell :: Iso' Cell (Maybe Player)
_Cell = _Newtype

updateCell :: Position -> Player -> GameState -> GameState
updateCell pos plyr
   = over _board 
   $ over _Board 
   $ over (at pos <<< _Just)
   $ set _Cell (Just plyr)

getPlayer :: Position -> Board -> Maybe Player
getPlayer pos board
   = view _Board board
   # view (at pos <<< _Just)
   # view _Cell

getCell :: Board -> Position -> Cell
getCell board pos
   = view _Board board
   # view (at pos <<< _Just)

_active :: Lens' GameState Player
_active = prop (SProxy :: SProxy "active")

updateActive :: (Player -> Player) -> GameState -> GameState
updateActive fn gs = over _active fn gs 