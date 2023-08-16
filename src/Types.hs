module Types where

import Data.Maybe

{-
    A coordinate represents a square on the board, it is a pair of integers (x,y).
    The board is 0-indexed, where (_,0) is the top row of the board, and (0,_) is
    the left-most column.
    The state of the board has a list of coordinates (of black and red pieces).
-}
type Coord = (Int, Int)
type PieceState = [Coord]
{-
    A move is essentially a list of coordinates, tracing out the path travelled
    by a piece, where you keep track of whether or not the piece is a Pawn or King
    using the PorK datatype.
    The list is "in the right order", e.g. [firstSquare, secondSquare, ...]
-}
data PorK a = P a | K a  
  deriving (Show,Eq, Read)

type Move = [PorK Coord]

{-
    The player datatype is the red/black colour.
-}
data Player = Red | Black -- 2 players, red and black
  deriving (Eq, Show)
{-
    It is either red/black's turn, or the game has finished.
-}
data Status = Turn Player | GameOver
  deriving (Eq,Show)
-- The status determines whether or not the game is still ongoing


-- Gamestate is a record and has all of the data for the state of a game.
data GameState =
  GameState { blackPieces :: PieceState
                   , redPieces :: PieceState
                   , blackKings :: PieceState
                   , redKings :: PieceState
                   , status :: Status
                   , message :: String 
                   , history :: [Move]}
              deriving (Show, Eq)


-- Your job is to write functions:
--   moves:: GameState -> ([Move],[Move])
--               to generate all the possible moves from a state
--               separated into the simple moves and the jump moves.
--                       (we will test this!!)
--   apply_move::CheckersEngine
--               to apply a move to generate the next state
--   GenMove:: the ai generates a move ...

type CheckersEngine = Move -> GameState -> GameState    -- to make a move
type GenMove = GameState -> Move  -- the AI generates a move


-- The initial game state

initialGameState :: GameState
initialGameState =
  GameState { blackPieces = blackInit
            , redPieces = redInit
            , blackKings = []
            , redKings = []
            , status = Turn Red
            , message = ""
            , history = []}

blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]
