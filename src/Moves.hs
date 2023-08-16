module Moves where

import Types

{-| Given a GameState of the current checker board this function returns a pair of all valid jump moves for the current player
and all valid simple moves for the current player

Parameters: GameState - A gamestate variable for the current checkerboard

Return: ([Move],[Move]) - The pair of valid simple moves for the current player and valid jump moves for the current player
-} 
moves :: GameState -> ([Move],[Move])
moves g = (simple_moves g, jump_moves g)

{-| Given a GameState of the current checker board, this function returns a list of all valid simple moves a player can make

Parameters: GameState - A gamestate variable for the current checkerboard

Return: [Move] - A list of simple moves the player can make
-}
simple_moves:: GameState -> [Move]
simple_moves st
    | _status st == (Just Red) = (simpleKing (redKings st))++(simplePiece (redPieces st)) -- Checking if the current player is red and finding the valid moves 
    | _status st == (Just Black) =  (simpleKing (blackKings st))++ (simplePiece (blackPieces st)) --  Checking if the current player is black and finding the valid moves
    | otherwise = []
    where
        {-| Given the PieceState of the kings, this function returns all possible simple moves with the kings

        Parameters: PieceState - The PieceState of the kings

        Return: [Move] - A list of possible simple moves with the kings pieces
        
        -}
        simpleKing :: PieceState -> [Move]
        simpleKing xs = filterHistory (history st) ([ [K (x,y), K (x',y')]      
                      | (x,y) <- xs                                             -- Iterating over all the kings pieces in the Piece State
                      , (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]        -- Iteratating over the 4 diagonals of the current king piece
                      , notoccupied (x',y') st && onboard (x',y')])      -- Checking if any of the diagnols are not occupied and on the game board


        {-| Given the PieceState of the pawns, this function returns all possible simple moves with the pawns

        Parameters: PieceState - The PieceState of the pawns

        Return: [Move] - A list of possible simple moves with the pawn pieces
        
        -}
        simplePiece :: PieceState -> [Move]
        simplePiece xs =
                      [ [P (x,y), (pToK (x',y') st)]
                      | (x,y) <- xs             -- Iterating over all the pawn pieces
                      , (x',y') <- let y'=y+dir in [(x+1,y'),(x-1,y')]  -- Interating over the forward diagonals of the pawns
                      , notoccupied (x',y') st && onboard (x',y')]      -- Checking if the forwar diagonals are not occupied and on the gameboard
                      where
                          dir
                            | _status st == (Just Red) = -1
                            | otherwise = 1

{-| Given a GameState of the current checker board, this function returns a list of all valid jump moves a player can make

Parameters: GameState - A gamestate variable for the current checkerboard

Return: [Move] - A list of jump moves the player can make
-}
jump_moves:: GameState -> [Move]
jump_moves st
    | _status st == (Just Red) = (jumpKing (redKings st))++(jumpPiece (redPieces st)) -- Getting jump moves for red player
    | _status st == (Just Black) =  (jumpKing (blackKings st))++ (jumpPiece (blackPieces st)) -- Getting jump moves for black player
    | otherwise = []
    where
        {-| This function returns all the jump moves for pawns

            Parameters: PieceState - The array of coordinates of pawns

            Return: [Move] - A list of jump moves for pawns
        -}
        jumpPiece :: PieceState -> [Move]
        jumpPiece xs = [(P (x,y)):ys | (x,y)<- xs, ys<- (jumpPiece' (x,y) [] (x,y))] -- Calling the helper function jumpPiece' with empty list

        {-| This is a helper function that returns the list of jump moves for pawns 

            Parameters: Coord -  The start coordinate of the jump moves
                        [Coord] - The list of positions that we have jumped over
                        Coord - The current position of the pawn

            Return: [Move] - List of jump moves
        -}
        jumpPiece' :: Coord -> [Coord] -> Coord -> [Move]
        jumpPiece' start xs (x,y)
            | (endBoard (x,y) st) = jumpKing' start xs (x,y) -- If the pieces is at the end of the board, we return the list of jump king moves
            | otherwise = [ 
                (pToK (x'',y'') st):ys
                | ((x',y'),(x'',y'')) <- [((x+1,y+dir),(x+2,y+2*dir)),((x-1,y+dir),(x-2,y+(2*dir)))] 
                -- Iterating through all possible positions for which we can jump and where the enemy position can be
                , not(member (x',y') xs) && opponent_occupied (x',y') st && (notoccupied (x'',y'') st || (x'',y'') == start) && (onboard (x'',y''))
                -- Checking if (x',y') is not a piece we have already captured, and (x',y') is an opponent piece and the position (x'',y'') is free
                , ys <- jumpover(jumpPiece' start ((x',y'):xs) (x'',y''))
                -- Recursively calling jumpPiece' to jump over and new pieces
            ]
            where 
                dir -- This function gets the forward direction of the pawn (-1 for red pieces and 1 for black piece)
                    | _status st == (Just Red) = -1
                    | otherwise = 1


        {-| This function returns all the jump moves for kings

            Parameters: PieceState - The array of coordinates of kings

            Return: [Move] - A list of jump moves for kings
        -}
        jumpKing :: PieceState -> [Move]
        jumpKing xs = [(K (x,y)):ys| (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y)]  -- Calling the helper function jumpKing' with empty list


        {-| This is a helper function that returns the list of jump moves for kings 

            Parameters: Coord -  The start coordinate of the jump moves
                        [Coord] - The list of positions that we have jumped over
                        Coord - The current position of the king

            Return: [Move] - List of jump moves for kings
        -}
        jumpKing' :: Coord -> [Coord] -> Coord -> [Move]
        jumpKing' start xs (x,y) = [
            (K (x'',y'')):ys
            | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
            -- Iterating through all possible positions for which we can jump and where the enemy position can be
            , not(member (x',y') xs) && opponent_occupied (x',y') st && (notoccupied (x'',y'') st || (x'',y'') == start) && (onboard (x'',y''))
            -- Checking if (x',y') is not a piece we have already captured, and (x',y') is an opponent piece and the position (x'',y'') is free
            , ys <- jumpover(jumpKing' start ((x',y'):xs) (x'',y'')) ]
            -- Recursively calling jumpKing' to jump over and new pieces

        jumpover [] = [[]]
        jumpover z = z

{-| This function takes a pair of coordinates and returns a boolean whether the pair is on the board or not

    Parameters: Coord - The pair of coordinates you want to check

    Return: Bool - True is returned if the coordinates are on the board and fale is returned otherwise

-}
onboard :: Coord -> Bool
onboard (x,y) = (x>=0 && x<8) && (y>=0 && y<8) -- checking if both x and y are between 0 and 7

{-| This function takes a paire of coordinates and returned a boolean whether the square corresponding to the coordinates is occupied or not

    Parameters: Coord - The poisiton on the gameboard you want to check

    Return: Bool - True if the spot is not occupied and false otherwise

-}
notoccupied :: Coord -> GameState -> Bool
notoccupied xs gs = not ((member xs (redKings gs)) || (member xs (redPieces gs)) || (member xs (blackKings gs)) || (member xs (blackPieces gs)))
-- Checking of the coordinate is a member of the redKings/redPieces/blackKings/blackPieces

{-| This function checks if the opponent occupies the coordinates specified

    Parameters: Coord - The coordinate that you want to check
                GameState - The GameState of the current checker board

    Return: Bool - True if the coordinate is occupied by the opponient and false otherwise

-}
opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied xs gs
    | _status gs == (Just Red)  = (member xs (blackKings gs)) || (member xs (blackPieces gs)) -- If the current player is red, we check if the coordinates is a member of the blackpieces
    | _status gs == (Just Black)  = (member xs (redKings gs)) || (member xs (redPieces gs))-- If the current player is black, we check if the coordinates is a member of the redpieces

{-| This function checks if a piece is at the end of the board (with repsect to its color)

    Parameters: Coord - The coordinate that we want to check
                GameState - The GameState of the current checker board
    
    Return: Bool - True if the input piece is at the end of the gameboard (with respect to the color) and false otherwise

-}
endBoard :: Coord -> GameState -> Bool 
endBoard (x,y) gs
    | _status gs == (Just Red) = y==0           -- If the current player is red, then y=0 is the end of the gameboard
    | _status gs == (Just Black) = y==7         -- If the current player is blakc, y= 7 is the end of the gameboard

test5 =  GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(6,7)]
                  , redKings = [(2,1), (3,2)]
                  , status = Turn Black
                  , message = "" 
                  , history = [[K (5,6),K (6,7)],[K (1,2),K (2,1)],[K (6,5),K (5,6)],[K (2,3),K (1,2)],[K (7,6),K (6,5)],[K (2,1),K (3,2)],[K (6,7),K (7,6)]]}

{-| This function checks if an element is a member of a list

    Parameters: a - The element you want to check
                [a] - The list you want to check 
    
    Return: Bool - True is returned if the input element is apart of the list, false is returned otherwise

-}
member :: Eq a => a -> [a] -> Bool
member a [] = False         -- No element is apart of the empty list
member a (x:xs) = (a==x) || member a xs -- Checking if the input element is equal to the head of the list, otherwise we continue seaching the rest of the list

{-| This function takes a GameState as input and returns the current player 

    Parameters: GameState - The GameState of the checkerboard

    Return: Maybe Player - Nothing is returned if the game is over, otherwise Just Red, or Just Black are returned as output

-}
_status :: GameState -> Maybe Player
_status gs = _status' (status gs)
    where
        _status' :: Status -> Maybe Player
        _status' (Turn p) = Just p
        _status' (GameOver) = Nothing

{-| This function checks if a pawn is at the end of the board and converts it to a PorK Coord

    Parameters: Coord - The pawn piece you want to check
                GameState - The gamestate of the current checkerboard

    Return: PorK Coord - If the pawn is at the end of the board we return K coord otherwise P coord is returned

-}
pToK :: Coord -> GameState -> PorK Coord
pToK as gs
    | (endBoard as gs) = K as -- Checking if the coordinate is at the end of the board if true, the pawn is promoted to a king
    | otherwise = P as         -- Otherwise the piece is returned as a pawn


{-| This function filters the Simple King moves in order to eliminate repeated states

    Parameters: [Move] - The history list
                [Move] - The list of simple kings moves

    Return: [Move] - The filtered simple king moves list

-}
filterHistory :: [Move] -> [Move] -> [Move]
filterHistory his [] = []       -- We return the empty list if there are no simple king moves
filterHistory his (x:xs)        -- We use pattern matching to get the top king move
    | isRepeatedState his [x] = filterHistory his xs    -- We check if the current move makes a repeated state
    | otherwise = x:(filterHistory his xs)              -- If it does not then we add it to the return list
    where

        {-| This function take takes a list of moves (history) and list of previous move current move pairs as input and checks if game state has been repeated
        
            Parameters: [Move] - The list of moves (history)
                        [Move] - list previous move current move pairs.

            Return: Bool - True is returned if the history has been repeated, false is returned otherwise
        
        -}
        isRepeatedState :: [Move] -> [Move] -> Bool
        isRepeatedState his [] = True 
        isRepeatedState ([K a, K b]:his) xs = isRepeatedState his (updateState [K a, K b] xs)
        isRepeatedState _ _ = False
        
        {-| This function updates the previous movecurrent move paris
        
            Parameters: Move - The current move in the history
                        [Move] - The input list of previous move current move pairs

            Return: [Move] - The updates list of previous move current move pairs
        -}
        updateState :: Move -> [Move] -> [Move]
        updateState xs [] = [xs]  
        updateState [K a, K b] ([K c, K d]:xs)
            | (b == c) && (d == a) = xs 
            | (b == c) = [K a, K d]:xs 
            | otherwise = [K c, K d]:(updateState [K a, K b] xs) 


        


        

