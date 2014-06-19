module LAB140606 where

import Data.Monoid 
import Data.List


donnees = [1.0, 10, 12.3, -25, 18.25]

{-f :: Double -> Maybe Double-}
{-f x | x < 0 = Nothing-}
    {-| otherwise = Just $ sqrt $ log x-}

{-[>Pas bonne solution!<]-}
{-
 -somme :: [Maybe Double] -> Double
 -somme [] = 0
 -somme (Nothing : xs) = somme xs
 -somme (Just y : xs) = y + somme xs
 -}


{-
 -somme :: [Maybe Double] -> Double
 -somme = foldr f 0
 -    where f Nothing valeur = valeur
 -          f (Just y) valeur = y + valeur
 -}

{-somme :: [Maybe Double] -> Double-}
{-somme = foldl' f 0-}
    {-where f valeur Nothing = valeur-}
          {-f valeur (Just y) = y + valeur-}

{-Avec Monoid-}
f :: Double -> Maybe Double
f x | x < 0 = Nothing
    | otherwise = Just $ sqrt $ log x

quantification :: Monoid a => Double -> (Double -> a) -> (a -> Double) -> Double
quantification e c get = get . (maybe (c e) id) . mconcat . map convert
    where convert = maybe Nothing (Just . c)

somme :: [Maybe Double] -> Double
somme = quantification 0 Sum getSum

produit :: [Maybe Double] -> Double
produit = quantification 1 Product getProduct



{-
 -somme :: [Maybe Double] -> Double
 -somme = getSum . (maybe (Sum 0) id) . mconcat . map convert
 -    where convert = maybe Nothing (Just . Sum)
 -
 -produit :: [Maybe Double] -> Double
 -produit = getProduct . (maybe (Product 0) id) . mconcat . map convert
 -    where convert = maybe Nothing (Just . Product)
 -}
