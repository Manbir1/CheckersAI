module Main where

import ApplyMove
import Moves
import ABsearch

import Checkers.Types
import Checkers.FrontEnd.Types
import Checkers.FrontEnd.Terminal

main :: IO()
{-
main =  frontend $ GameConfig { engine = apply_move
                                                     , blackMove = Human
                                                     , redMove = Human
                                                     , state = initialGameState }
-}

main =  frontend $ GameConfig { engine = apply_move
                                                    , blackMove = Ai (black_ai)
                                                    , redMove = Ai (red_ai)
                                                    , state = initialGameState }
