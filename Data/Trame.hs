module Data.Trame 
    ( Trame 
    -- * Constructeur et accesseurs 
    , pixels 
    , creer
    -- * Fonction utile
    , ordonner 
    ) where

import Data.Couleur (Couleur(..)) 
import qualified Data.Couleur as Couleur

import Data.Pixel (Pixel) 
import qualified Data.Pixel as Pixel 

import Data.Point (Point) 
import qualified Data.Point as Point

import Data.Monoid 
import qualified Data.List as List 

data Trame = T {
      {- | @'pixels' t@ extrait la liste des pixels contenus dans la trame @t@ -}
      pixels :: [Pixel] 
} deriving (Eq, Ord, Show, Read)

{- | @'creer' pxs@ crée une nouvelle trame à partir de @pxs@ -}
creer :: [Pixel] -> Trame 
creer pxs = T $ ordonner pxs

{- | @'ordonner' pxs@ classe les éléments de pxs en ordre croissant tout en supprimant les doublons -}
ordonner :: [Pixel] -> [Pixel] 
ordonner = List.nubBy eqPos . List.sortBy ordPos


{- Fonctions utiles -}
px1 `eqPos` px2  = Pixel.position px1 == Pixel.position px2 
px1 `ordPos` px2 = compare (Pixel.position px1) (Pixel.position px2) 

