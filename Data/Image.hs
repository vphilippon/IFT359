module Data.Image 
    ( Image
    -- * Constructeur et accesseurs 
    , creer
    , entete
    , trame
    ) where

import Data.Couleur 

import Data.Entete (Entete)
import qualified Data.Entete as Entete 

import Data.Point (Point,Coordonnee)
import qualified Data.Point as Point 

import Data.Pixel (Pixel (..))
import qualified Data.Pixel as Pixel

-- chargement de Data.Trame : la deuxième ligne permet d'utiliser les opérateurs sans préfixe
import Data.Trame (Trame)
import qualified Data.Trame as Trame 

-- cache l'opérateur >= du prélude 

data Image = I { 
    -- | @'entete' i@ extrait l'entête de l'image @i@
    entete :: Entete
    -- | @'trame' i@ extrait la trame de l'image @i@
  , trame  :: Trame 
} deriving (Eq,Ord,Read,Show)

{- | @'creer' e t@ crée une nouvelle image à partir de l'entête @e@ et la trame @t@ -}
creer :: Entete -> Trame -> Image 
creer = I


