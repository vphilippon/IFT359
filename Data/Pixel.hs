module Data.Pixel 
    ( Pixel
    , creer
    , position
    , couleur
    ) where

import Data.Point (Point)
import Data.Couleur (Couleur) 

data Pixel = Px { position :: Point
                , couleur :: Couleur
                }
    deriving (Ord, Eq, Read) 

instance Show Pixel where
    show px = "px:" ++ (show $ position px) ++ ":" ++ (show $ couleur px) 

creer :: Point -> Couleur -> Pixel
creer = Px 

