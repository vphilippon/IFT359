module Data.Entete 
    ( Entete
    , Coordonnee
    -- * Constructeur et accesseurs 
    , creer 
    , hauteur
    , largeur
    ) where

import Data.Point (Coordonnee)

data Entete = E { hauteur :: Coordonnee
                , largeur :: Coordonnee 
                }
    deriving (Eq, Ord, Read)

instance Show Entete where
    show (E h l) = show h ++ "x" ++ show l

creer :: Coordonnee -> Coordonnee -> Entete
creer = E

