module Heuristic where

import Moves
import ApplyMove
import Checkers.Types

heuristic :: GameState -> Player -> Float 
heuristic g play 
    -- The idea for the opening is that we want to keep as many pawns in the back rank, and secure some central pawns.
    -- We also try to secure a slight pawn lead
    | (openingGame g) = fromIntegral(3*((number_of_P g play) - (number_of_P g (opposite play)))  + 
    ((number_of_backRow g play) - (number_of_backRow g (opposite play))) + 2*(number_of_K g play) 
    + (number_of_central_P g play)- (number_of_central_P g (opposite play))) 
    -- For the midgame, we want to keep the number of kings we have as high as we can
    | (midGame g) = 
        fromIntegral((3*((number_of_K g play) - (number_of_K g (opposite play)))) + ((number_of_P g play) - (number_of_P g (opposite play))))
    -- For the endgame, we want to move out remaining pawns to the oponents side to update to kings, we also want to eliminate all of the oponents
    -- kings before we eliminate the pawns
    | otherwise = fromIntegral(5*((number_of_K g play) - (number_of_K g (opposite play))) + 2*((number_of_P g play) - (number_of_P g (opposite play))))
        + (0.15*fromIntegral(dist_from_K g play))

opposite :: Player -> Player
opposite Red =  Black 
opposite Black = Red

openingGame :: GameState -> Bool 
openingGame g = (number_of_P g Black)>3 && (number_of_P g Red)>3 && (number_of_K g Black) == 0 && (number_of_K g Red) == 0


midGame :: GameState -> Bool
midGame g  = (number_of_P g Black)>3 && (number_of_P g Red)>3 && ((number_of_K g Black) > 0 || (number_of_K g Red)> 0)

black_heuristic :: GameState -> Float
black_heuristic gs = fromIntegral(length (blackPieces gs) - length (redPieces gs) + 2*(length (blackKings gs) - length (redKings gs)))

red_heuristic :: GameState -> Float
red_heuristic gs = fromIntegral(length (redPieces gs) - length (blackPieces gs) + 2*(length (redKings gs) - length (blackKings gs)))

number_of_P :: GameState -> Player -> Int
number_of_P gs Black = length (blackPieces gs)
number_of_P gs Red = length (redPieces gs)

number_of_K :: GameState -> Player -> Int
number_of_K gs Black = length (blackKings gs)
number_of_K gs Red = length (redKings gs)

number_of_backRow :: GameState -> Player -> Int
number_of_backRow gs Black = foldr (\(x,y) acc -> if y==0 then acc+1 else acc) 0 (blackPieces gs)
number_of_backRow gs Red = foldr (\(x,y) acc -> if y==7 then acc+1 else acc) 0 (redPieces gs)

number_of_central_P :: GameState -> Player -> Int 
number_of_central_P gs Black = foldr (\(x,y) acc -> if (2<=x && x<=5) && (3<=y && y<=4) then acc+1 else acc) 0 (blackPieces gs)
number_of_central_P gs Red = foldr (\(x,y) acc -> if (2<=x && x<=5) && (3<=y && y<=4) then acc+1 else acc) 0 (redPieces gs)

dist_from_K :: GameState -> Player -> Int 
dist_from_K gs Black = foldr(\(x,y) acc -> (y-7)) 0 (blackPieces gs)
dist_from_K gs Red = foldr(\(x,y) acc -> (-y)) 0 (redPieces gs)