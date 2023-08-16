module ApplyMove where

import Moves 
import Checkers.Types

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