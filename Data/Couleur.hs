module Data.Couleur 
    ( Couleur (..) 
    ) where

data Couleur = RGB { rouge :: Int
                   , vert  :: Int
                   , bleu  :: Int
                   }
    deriving (Eq, Ord, Read) 

instance Show Couleur where
    show (RGB r v b) = "RGB " ++ show r ++ " " ++ show v ++ " " ++ show b

