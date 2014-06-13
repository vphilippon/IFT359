module LAB140613 where

import Control.Applicative
import Data.List
import Data.Monoid

-- Exercice 1
newtype Min a = Min { getMin :: a }

instance Functor Min where
    fmap f = Min . f . getMin
    {-fmap = ($)-}

instance Applicative Min where
    pure = Min
    (<*>) (Min f) = pure . f . getMin
    {-(<*>) = getMin-}

instance (Bounded a, Ord a) => Monoid (Min a) where 
    mempty = Min maxBound
    --mappend (Min x) (Min y) = Min $ min x y
    mappend = liftA2 min 

-- exercice 2
minimum :: [Int] -> Int
minimum = foldl' min maxBound -- foldl'

minimum' :: [Int] -> Int
{-minimum' xs = getMin $ mconcat $ map Min xs-}
minimum' = getMin . mconcat . map Min -- Monoid

-- exercice 3 
data BTree a = Leaf a | Node a (BTree a) (BTree a)

racine :: BTree a -> a 
racine = undefined

left :: BTree a -> BTree a 
left = undefined

right :: BTree a -> BTree a 
right = undefined

isLeaf :: BTree a -> Bool
isLeaf = undefined

isNode :: BTree a -> Bool
isNode = undefined

-- exercice 4
instance Functor BTree where
    fmap  = undefined 

instance Applicative BTree where
    pure  = undefined 
    (<*>) = undefined 

-- exercice 5
-- Parcours 
