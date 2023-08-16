module ABsearch where

import Moves
import ApplyMove
import Checkers.Types
import Heuristic


{-| This function uses the minimax algorithm with a-bpruning to find moves for black

    Parameters: GameState - The current gamestate

    Return: Move -  The best move for black that is determined by out heuristic function
-}
black_ai :: GameState -> Move
black_ai g = case snd (abmaxprune g (bot,top) 6 Black) of
    Just m -> m 
    Nothing -> []

{-| This function uses the minimax algorithm with a-bpruning to find moves for red

    Parameters: GameState - The current gamestate

    Return: Move -  The best move for red that is determined by out heuristic function
-}
red_ai :: GameState -> Move
red_ai g = case snd (abmaxprune g (bot,top) 6 Red) of 
    Just m -> m 
    Nothing -> []

-- constant
top :: Float
top = 3000000000

-- constant
bot :: Float
bot = -3000000000

{-| This function returns a list of moves (If a jump move is available, we return the list of jump moves, other we return the list of simple moves)

    Parameters: GameState - The current gamestate

    Return: Move -  The list of moves
-}
abmoves :: GameState -> [Move]
abmoves g = if snd (moves g) == [] then fst (moves g) else snd (moves g)

{-| This function wheather or not a jump omve is available

    Parameters: GameState - The current gamestate

    Return: Bool - True if a jump move is not avaliable, and false otherwise
-}
notJumpMove :: GameState -> Bool 
notJumpMove g = if snd (moves g) == [] then True else False 

{- This function implements A-B Pruning for the minimax alogrithm. This function implements the 
max pruning part of the algorithm

    Parameters: Gamestate - The current gamestate
                (Float, Float) - The starting alpha and beta values
                Int - The minimum depth that we are searching
                Player - The player we are maximizing for

    Return: (Float, Maybe Move) - A tuple that contains the maximum heuristic value obtained from the minimax 
                and the move that goes towards that heuristic
-}
abmaxprune :: GameState -> (Float, Float) -> Int -> Player -> (Float, Maybe Move)
abmaxprune g (alpha, beta) depth play 
    | status g == GameOver = (heuristic g play, Nothing)                    -- Checking if the game is over
    | (depth <= 0) && notJumpMove g =  (heuristic g play, Nothing)              -- Quiescence, we only evaluate the heuristic at the optimal depth and when no jump moves are available
    | otherwise = snd (abmaxprunes (abmoves g) (alpha, (bot, Just (head (abmoves g))))) -- Calling abmaxprunes to get the best move
    where
        abmaxprunes :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
        abmaxprunes [] curr = curr
        abmaxprunes (x:xs) (prevAlpha, (prevValue, prevMove))       -- Iterating through the array of moves to find the best branch of moves
            | newAlpha >= beta = (newAlpha, (newVal, newMove))      -- When the alpha value is greater than beta, there is no point searching any further, so we stop the search here
            | otherwise = abmaxprunes xs (newAlpha, (newVal, newMove)) -- Iterating through the rest of the list
                where
                    eval = fst (abminprune (apply_move x g) (prevAlpha, beta) (depth-1) play)   -- Evaluating the current branch (calling abminprune because opponent trys to minimize heuristic)
                    newVal = max prevValue eval                                                 -- The new value will be the max of the previous max branch and the current
                    newAlpha = max prevAlpha newVal                                             -- The new alpha will be the maximum between the previous and current
                    newMove = if prevValue == newVal then prevMove else Just x                  -- Checking if the current branch provides more favorable outcome then the previous

{- This function implements A-B Pruning for the minimax alogrithm. This function implements the 
min pruning part of the algorithm

    Parameters: Gamestate - The current gamestate
                (Float, Float) - The starting alpha and beta values
                Int - The minimum depth that we are searching
                Player - The player we are minimizing for

    Return: (Float, Maybe Move) - A tuple that contains the minimum heuristic value obtained from the minimax 
                and the move that goes towards that heuristic
-}
abminprune :: GameState -> (Float, Float) -> Int -> Player -> (Float, Maybe Move)
abminprune g (alpha, beta) depth play 
    | status g == GameOver = (heuristic g play, Nothing)         -- Checking if the game is over
    | (depth <= 0) && notJumpMove g =  (heuristic g play, Nothing)  -- Quiescence, we only evaluate the heuristic at the optimal depth and when no jump moves are available
    | otherwise = snd (abminprunes (abmoves g) (beta, (top, Just (head (abmoves g))))) -- Calling abminprunes to find the optimal moves for the opposing player
    where
        abminprunes :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
        abminprunes [] curr = curr
        abminprunes (x:xs) (prevBeta, (prevValue, prevMove))
            | alpha >= newBeta = (newBeta, (newVal, newMove))  -- When the alpha value is greater than beta, there is no point searching any further
            | otherwise = abminprunes xs (newBeta, (newVal, newMove)) -- Iterating through the rest of the list
                where
                    eval = fst (abmaxprune (apply_move x g) (alpha, prevBeta) (depth-1) play) -- Evaluating the current branch (calling abmaxprune because the current player will try to maximize the the heuristic)
                    newVal = min prevValue eval                                               -- The new value will be the min of the previous min branch and the current
                    newBeta = min prevBeta newVal                                             -- The new beta will be the minimum between the previous and current
                    newMove = if prevValue == newVal then prevMove else Just x                -- Checking if the current branch provides more favorable outcome then the previous

