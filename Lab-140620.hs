module LAB140620 where

import Data.Automate
import Data.Programme
import Control.Executable

{- PROGRAMME -}
-- prog1 :: Programme String Char 
prog1 = P "abcde" id

-- prog2 :: Programme String Char 
prog2 = P "abcde" echec
  where -- echec :: [a] -> b
        -- echec :: String -> String
        echec etat = "Echec avec " ++ etat 

-- prog3 :: Programme String Integer 
prog3 = P [1,2,3,4] echec
  where -- echec :: [a] -> b
        -- echec :: [Integer] -> String
        echec etat = "Echec avec " ++ show etat 

{- AUTOMATE -}
aut1 = AFD "Automate1" delta "q0" ("q1"==) 
    where delta "q0" 'a' = Just "q1"
          delta "q1" 'b' = Just "q2"
          delta "q2" 'c' = Just "q0"
          delta _ _ = Nothing 

aut2 = AFD "Automate 2" delta "q0" ("q1"==) 
    where delta "q0" 'a' = Just "q1"
          delta "q1" 'b' = Just "q2"
          delta "q2" 'b' = Just "q0"
          delta _ _ = Nothing 
{-
testAut1 = let resultat1 = execute aut1 'a'
               resultat2 = execute' aut1 "ab"
               resultat3 = execute' aut1 "abb"
           in mapM_ print [resultat1, resultat2, resultat3] 

testAut2 = let resultat1 = execute aut2 'a'
               resultat2 = execute' aut2 "ab"
               resultat3 = execute' aut2 "abb"
           in mapM_ print [resultat1, resultat2, resultat3] 
-}


appliquerTests :: (Eq a, Show a, Executable t, Show (t a)) => [t a] -> [[a]] -> IO ()
appliquerTests executables commandes = mapM_ print resultats
    where resultats = [ (e, c, execute' e c) | e <- executables, c <- commandes ]

testsAutomates = let automates = [aut1, aut2] 
                     commandes = ["a", "ab", "abb", "abc"]
                 in appliquerTests automates commandes 


