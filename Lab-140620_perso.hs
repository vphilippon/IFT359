module LAB20140620 where

import Control.Executable
import Data.Programme

prog1 :: Programme String Char
prog1 = P "abcde" id

prog2 :: Programme String Char
prog2 = P "abcde" echec
    where -- [a] -> b
          -- String -> String
          echec etat = "Echec avec " ++ etat

prog3 :: Programme String Char
prog3 = 
