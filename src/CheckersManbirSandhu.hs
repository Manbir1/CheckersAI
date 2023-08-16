module CheckersRobinCockett (moves, apply_move, red_ai, black_ai)

where 

import Checkers.Types

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
        isRepeatedState his [] = True -- If the previous move current move list is empty, then a repeated move has occured
        isRepeatedState ([K a, K b]:his) xs = isRepeatedState his (updateState [K a, K b] xs) -- updating the previous move current move list and calling the isRepeatedState function
        isRepeatedState _ _ = False -- Otherwise the history contains a Pawn move or a jump king move, so the state cannot be repeated
        
        {-| This function updates the previous movecurrent move paris
        
            Parameters: Move - The current move in the history
                        [Move] - The input list of previous move current move pairs

            Return: [Move] - The updates list of previous move current move pairs
        -}
        updateState :: Move -> [Move] -> [Move]
        updateState xs [] = [xs]        -- Case where the list of previous move current move pairs is empty
        updateState [K a, K b] ([K c, K d]:xs)
            | (b == c) && (d == a) = xs -- If updating the state results in the previous move equaling the current move, we remove it from the list
            | (b == c) = [K a, K d]:xs -- Checking if the current move ends where the previous move starts, and updating the list accordingly
            | otherwise = [K c, K d]:(updateState [K a, K b] xs) -- Otherwise we update the rest of the state


{-| This function takes a move and a gamestate as input and applys the move (if the move is valid)

    Parameters: CheckesEngine (Move -> GameState)

    Return: This function returns a GameState after the move has been processed

-}
apply_move :: CheckersEngine
apply_move mv st 
    | moves st == ([],[]) = st{status = GameOver}
    | inmoves mv (fst (moves st)) = if length (snd (moves st))==0 then make_simple_move mv st{history = mv:(history st)} 
    else st{message = ("Illegal move! A jump is available:  " ++ show(snd (moves st)))}
    -- Checking if the simple move is a valid simple move (and no jump moves are available) and processing the move accordinly
    | inmoves mv (snd (moves st)) = make_jump_move mv st{history = mv:(history st)} -- Checking if the jump move is valid and making the jump move if it is
    | otherwise = st{message = show(moves st)} -- Otherwise the move is invalid and we update the message

{-| This function takes a simple move and a GameState and returns the new gamestate after the move has been applied

    Parameters: Move - The simple move that we will apply
                GameState - The current GameState of the checkerboard

    Return: GameState - The new gamestate after the move has been applied
-}
make_simple_move :: Move -> GameState -> GameState
make_simple_move ([K start,end]) st -- Case for simple king move
    | (_status st == (Just Red)) -- Case for red king
        = st{redKings = replace start (unPackMove end) (redKings st) -- replace red king new position and change player
            , status = change_player st
            , message = ""}
    |(_status st == (Just Black)) -- Case for black king
        = st{blackKings = replace start (unPackMove end) (blackKings st) -- replace black king to new position and change player
            , status = (change_player st)
            , message = ""}

make_simple_move ([P start, P end]) st -- Case for Pawn -> Pawn
    |(_status st == (Just Black)) -- Case for black pawns
        = st{blackPieces = replace start end (blackPieces st) -- replace black pawn to new position and change players
            , status = change_player st
            , message = ""}
    |(_status st == (Just Red)) -- Case for red pawn
        = st{redPieces = replace start end (redPieces st) -- replace red pawn to new position and change players
            , status = change_player st
            , message = ""}

make_simple_move ([P start, K end]) st  -- Case for Pawn -> King
    |(_status st == (Just Black)) -- Case for black pawns turning into black kings
        = st{blackPieces = remove start (blackPieces st) -- removing the black pawn
            , blackKings = end:(blackKings st) -- Adding a black king at the new position
            , status = change_player st -- Changing players
            , message = ""}
    |(_status st == (Just Red)) -- Case for red pawns turning into red kings
        = st{redPieces = remove start (redPieces st) --  Removing the red pawn
            , redKings = end:(redKings st)  -- Adding a red king at the new position
            , status = change_player st -- Changing players
            , message = ""}


{-| This function takes a jump move and a GameState and returns the new gamestate after the move has been applied

    Parameters: Move - The jump move that we will apply
                GameState - The current GameState of the checkerboard

    Return: GameState - The new gamestate after the move has been applied
-}
make_jump_move :: Move -> GameState -> GameState
make_jump_move [_] st = st{status = change_player st}
make_jump_move ((K start):(next:rest)) st -- Case for jump move for kings
    | (_status st == (Just Red)) -- Case for Red player
        = make_jump_move (next:rest) 
        (st{blackKings = remove (jumped start (unPackMove next)) (blackKings st) -- Removing a black king (if a king was jumped over)
            , blackPieces = remove (jumped start (unPackMove next)) (blackPieces st) -- Removing a black pawn (if a black pawn was jumped over)
            , redKings = (unPackMove next):(remove start (redKings st)) -- Moving the red king into the new position
            , message = ""})
    | (_status st == (Just Black)) -- Case for Black Player
        = make_jump_move (next:rest) 
        (st{redKings = remove (jumped start (unPackMove next)) (redKings st) -- Removing a red king (if a king was jumped over)
            , redPieces = remove (jumped start (unPackMove next)) (redPieces st) -- Removing a red pawn (if a black pawn was jumped over)
            , blackKings = (unPackMove next):(remove start (blackKings st)) -- Moving the black king into the new position
            , message = ""})     
make_jump_move ((P start):((P next) :rest)) st -- Case for Pawn jumping to new Pawn position
    | (_status st == (Just Red)) -- Case for Red player
        = make_jump_move ((P next):rest) 
        (st{blackKings = remove (jumped start next) (blackKings st) -- Removing a black king (if a king was jumped over)
            , blackPieces = remove (jumped start next) (blackPieces st) -- Removing a black pawn (if a black pawn was jumped over)
            , redPieces = next:(remove start (redPieces st))  -- Moving the red pawn into the new position
            , message = ""})
    | (_status st == (Just Black)) -- Case for Black player
        = make_jump_move ((P next):rest) 
        (st{redKings = remove (jumped start next) (redKings st) -- Removing a red king (if a king was jumped over)
            , redPieces = remove (jumped start next) (redPieces st) -- Removing a red pawn (if a black pawn was jumped over)
            , blackPieces = next:(remove start (blackPieces st)) -- Moving the black pawn into the new position
            , message = ""})
make_jump_move ((P start):((K next) :rest)) st -- Case for Pawn transforming to King
    | (_status st == (Just Red)) -- Case for Red Player
        = make_jump_move ((K next):rest)
        (st{blackKings = remove (jumped start next) (blackKings st) -- Removing a black king (if a king was jumped over) 
            , blackPieces = remove (jumped start next) (blackPieces st) -- Removing a black pawn (if a black pawn was jumped over)
            , redPieces = remove start (redPieces st)      -- Removing the pawn
            , redKings = next:(redKings st) -- Adding a new red king
            , message = "" })
    | (_status st == (Just Black))
        = make_jump_move ((K next):rest)
        (st{redKings = remove (jumped start next) (redKings st) -- Removing a red king (if a king was jumped over)
            , redPieces = remove (jumped start next) (redPieces st) -- Removing a red pawn (if a black pawn was jumped over)
            , blackPieces = remove start (blackPieces st) -- Removing the pawn
            , blackKings = next:(blackKings st) -- Adding the new black king
            , message = "" })

{-| This function unpacks a PorK Coord data type into a Coord data type

    Parameters: PorK Coord - The PorK Coord data type type you want to unpack

    Return: Coord - The unpack coordinate

-}
unPackMove :: PorK Coord -> Coord
unPackMove (P a) = a
unPackMove (K a) = a

{-| This function checks if a move is in a list of moves

    Parameters: Move - The move you are checking
                [Move] - The list of moves that you want to 

    Return: Bool - True is the move is in the list of moves, false others
-}
inmoves :: Move -> [Move] -> Bool
inmoves mv x = (member mv x)        -- We check if the mv is a member of the list of moves

{-| This function takes two coordinates, start position of a jumped move and the end position, and it returns
    the coordinate of the piece jumped over

    Parameters: Coord - The beginning position of the jump move
                Coord - The end position of the jump move

    Return: Coord - The coordinate of the position jumped over
-}
jumped :: Coord -> Coord -> Coord
jumped (x,y) (a,b)  -- Evaluating each case and returning one of the four corners from the start coordinate
    | (a-x) == 2 && (b-y) == 2 = (x+1,y+1)         
    | (a-x) == 2 && (b-y) == (-2) = (x+1,y-1)
    | (a-x) == (-2) && (b-y) == 2 = (x-1,y+1)
    | otherwise = (x-1,y-1)

{-| This function takes an element and a list and removes the first occurence of the input element from the list

    Parameters: a - The element you want to delete from the list
                [a] - The list which you want to delete from

    Return: [a] - The list with the first occurence of the input element removed
-}
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a (x:xs)
    | a == x = xs 
    | otherwise = x:(remove a xs)

{-| This function takes an element and a replacement element, and it replaces the first occurenece of the element in the list 
    with the replacement element

    Parameters: a - The element that is to be replaces
                a - The replacement element that will replace the previous element
                [a] - The list which we ant to modify

    Return: [a] - The new list with the first occurence of the input element replaces with the replacement element
-}
replace :: Eq a => a -> a -> [a] -> [a] 
replace x y [] = []     -- Case for the empty list
replace x y (a:as)    
    | a == x = y:as -- Checking if x is equal to the head of the list. If true, then we return y:as
    | otherwise = a:(replace x y as) -- Otherwise we keep the current element and replace the rest of the list

{-| This function returns the next player that is supposed to play

    Parameters: GameState - The gamestate of the current checkerboard

    Return: Status - The new status(who will play) is returned
-}
change_player :: GameState -> Status
change_player s
    | _status s == (Just Red) = (Turn Black) -- If the current player is red, then the next turn is black
    | otherwise = (Turn Red)    -- If the current player is black, then the next turn is red

{-| This function uses the minimax algorithm with a-bpruning to find moves for black

    Parameters: GameState - The current gamestate

    Return: Move -  The best move for black that is determined by out heuristic function
-}
black_ai :: GameState -> Move
black_ai g = case snd (abmaxprune g (bot,top) 7 Black) of
    Just m -> m 
    Nothing -> []

{-| This function uses the minimax algorithm with a-bpruning to find moves for red

    Parameters: GameState - The current gamestate

    Return: Move -  The best move for red that is determined by out heuristic function
-}
red_ai :: GameState -> Move
red_ai g = case snd (abmaxprune g (bot,top) 7 Red) of 
    Just m -> m 
    Nothing -> []

-- constant
top :: Float
top = 300000

-- constant
bot :: Float
bot = -300000

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

abmaxprune :: GameState -> (Float, Float) -> Int -> Player -> (Float, Maybe Move)
abmaxprune g (alpha, beta) depth play 
    | status g == GameOver = (heuristic g play, Nothing)                    -- Checking if the game is over
    | (depth <= 0) && notJumpMove g =  (heuristic g play, Nothing)              -- Quiescence, we only evaluate the heuristic at the optimal depth and when no jump moves are available
    | otherwise = snd (abmaxprunes (abmoves g) (alpha, (bot, Just (head (abmoves g))))) -- Calling abmaxprunes to get the best move
    where
        abmaxprunes :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
        abmaxprunes [] curr = curr
        abmaxprunes (x:xs) (prevAlpha, (prevValue, prevMove))       -- Iterating through the array of moves to find the best branch of moves
            | newAlpha >= beta = (newAlpha, (newVal, newMove))
            | otherwise = abmaxprunes xs (newAlpha, (newVal, newMove))
                where
                    eval = fst (abminprune (apply_move x g) (prevAlpha, beta) (depth-1) play)   -- Evaluating the current branch (calling abminprune on the current move)
                    newVal = max prevValue eval                                                 -- The new value will be the max of the previous max branch and the current
                    newAlpha = max prevAlpha newVal                                             -- THe new alpha will be the maximum between the previous and current
                    newMove = if prevValue == newVal then prevMove else Just x                  -- Checking if the current branch provides more favorable outcome then the previous

abminprune :: GameState -> (Float, Float) -> Int -> Player -> (Float, Maybe Move)
abminprune g (alpha, beta) depth play 
    | status g == GameOver = (heuristic g play, Nothing)
    | (depth <= 0) && notJumpMove g =  (heuristic g play, Nothing)
    | otherwise = snd (abminprunes (abmoves g) (beta, (top, Just (head (abmoves g)))))
    where
        abminprunes :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
        abminprunes [] curr = curr
        abminprunes (x:xs) (prevBeta, (prevValue, prevMove))
            | alpha >= newBeta = (newBeta, (newVal, newMove))
            | otherwise = abminprunes xs (newBeta, (newVal, newMove))
                where
                    eval = fst (abmaxprune (apply_move x g) (alpha, prevBeta) (depth-1) play)
                    newVal = min prevValue eval 
                    newBeta = min prevBeta newVal
                    newMove = if prevValue == newVal then prevMove else Just x

{-| This function returns the evaluation of the gamestate the specified player

    Parameters: GameState - The current gamestate
                Player - The player we are evaluating for

    Return: Float - The 
-}
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

{-| This function returns the opposite player

    Parameters: Player - The current player

    Return: Player - The opposite player
-}
opposite :: Player -> Player
opposite Red =  Black 
opposite Black = Red

{-| This function returns wheather the game is in the opening state

    Parameters: GameState - The state of the current game

    Return: True if the game is in the opening phase, false otherwise
-}
openingGame :: GameState -> Bool 
openingGame g = (number_of_P g Black)>3 && (number_of_P g Red)>3 && (number_of_K g Black) == 0 && (number_of_K g Red) == 0


{-| This function returns wheather the game is in the mid game

    Parameters: GameState - The state of the current game

    Return: True if the game is in the mid game phase, false otherwise
-}
midGame :: GameState -> Bool
midGame g  = (number_of_P g Black)>3 && (number_of_P g Red)>3 && ((number_of_K g Black) > 0 || (number_of_K g Red)> 0)

black_heuristic :: GameState -> Float
black_heuristic gs = fromIntegral(length (blackPieces gs) - length (redPieces gs) + 2*(length (blackKings gs) - length (redKings gs)))

red_heuristic :: GameState -> Float
red_heuristic gs = fromIntegral(length (redPieces gs) - length (blackPieces gs) + 2*(length (redKings gs) - length (blackKings gs)))

{-| This function returns the number of pawn pieces on the board for the input player

    Parameters: GameState - The state of the current game
                Player - The color of the current player

    Return: Int - the number of pawn pieces the input player has
-}
number_of_P :: GameState -> Player -> Int
number_of_P gs Black = length (blackPieces gs)
number_of_P gs Red = length (redPieces gs)

{-| This function returns the number of king pieces on the board for the input player

    Parameters: GameState - The state of the current game
                Player - The color of the current player

    Return: Int - the number of king pieces the input player has
-}
number_of_K :: GameState -> Player -> Int
number_of_K gs Black = length (blackKings gs)
number_of_K gs Red = length (redKings gs)

{-| This function returns the number of pawn pieces that are on the back rank for the current player

    Parameters: GameState - The state of the current game
                Player - The color of the current player

    Return: Int - the number of pawn pieces on the back rank
-}
number_of_backRow :: GameState -> Player -> Int
number_of_backRow gs Black = foldr (\(x,y) acc -> if y==0 then acc+1 else acc) 0 (blackPieces gs)
number_of_backRow gs Red = foldr (\(x,y) acc -> if y==7 then acc+1 else acc) 0 (redPieces gs)

{-| This function returns the number of pawn pieces that are "central" pawns (that is they control the center)

    Parameters: GameState - The state of the current game
                Player - The color of the current player

    Return: Int - The number of central pawns
-}
number_of_central_P :: GameState -> Player -> Int 
number_of_central_P gs Black = foldr (\(x,y) acc -> if (2<=x && x<=5) && (3<=y && y<=4) then acc+1 else acc) 0 (blackPieces gs)
number_of_central_P gs Red = foldr (\(x,y) acc -> if (2<=x && x<=5) && (3<=y && y<=4) then acc+1 else acc) 0 (redPieces gs)

{-| This function returns the total distance from each pawn piece to the end of the board

    Parameters: GameState - The state of the current game
                Player - The color of the current player

    Return: Int - The total distance from each pawn piece to the end of the board
-}
dist_from_K :: GameState -> Player -> Int 
dist_from_K gs Black = foldr(\(x,y) acc -> (y-7)) 0 (blackPieces gs)
dist_from_K gs Red = foldr(\(x,y) acc -> (-y)) 0 (redPieces gs)