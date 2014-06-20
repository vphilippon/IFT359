module Data.Programme where 

import Control.Executable

data Programme b a = P { commandes :: [a], echecAvec :: [a] -> b}

instance Show b => Executable (Programme b) where
    execute (P [] e) _ = Left $ show $ e []
    execute (P l@(x:xs) e) y | x == y    = Right (P xs e)
                             | otherwise = Left $ show $ e l

instance Show a => Show (Programme b a) where
    show (P cs _) = show cs 

