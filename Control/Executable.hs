module Control.Executable where

import Data.List (foldl') 

type Execution t = Either String t

class Executable t where 
    execute  :: Eq a => t a -> a -> Execution (t a)

    execute' :: Eq a => t a -> [a] -> Execution (t a)
    execute' e = foldl' continue depart
        where depart = Right e
              continue l@(Left _) = const l
              continue (Right e) = execute e

