module Lab140515 where 

import Data.Maybe

-- calculer la somme d'une liste de nombres
-- somme [1,2,3,4]
somme :: Num a => [a] -> a
somme [] = 0
somme (x:xs) = x + somme xs

-- calculer le produit d'une liste de nombres
-- produit [1,2,3,4]
produit :: Num a => [a] -> a
produit [] = 1
produit (x:xs) = x * produit xs

-- evaluer la verite de toutes les valeurs de verite
-- pourtout [(1==2), False, True]
pourtout :: [Bool] -> Bool
pourtout [] = True
pourtout (b:bs) = b && pourtout bs 

-- evaluer la verite d'au moins une valeur de verite
-- ilexiste [(1==2), False, True]
ilexiste :: [Bool] -> Bool
ilexiste [] = False
ilexiste (b:bs) = b || ilexiste bs 

-- calculer position
position :: (Eq a) => a -> [a] -> Maybe Int
position x list = positionDe 0 x list

positionDe :: (Eq a) => Int -> a -> [a] -> Maybe Int 
positionDe n y [] = Nothing
positionDe n y (x:xs)
    | x == y    = Just n
    | otherwise = positionDe (n+1) y xs

-- calculer integrale 
{-integrale :: [Double] -> Double -> Double -> Double-}
{-integrale valeurs a b = undefined -}

-- calculer le milieu de deux valeurs 
milieu :: Double -> Double -> Double
milieu x y = (x + y) / 2

-- calculer la moyenne d'une liste de valeur 
moyenne :: (Fractional a) => [a] -> a
moyenne xs = somme xs / fromIntegral (length xs)


{-sommeEtCompte :: Num a => [a] -> (a,Int)-}
{-sommeEtCompte [] = (0,0)-}
{-sommeEtCompte (x:xs) = (x,1) + sommeEtCompte xs-}
    {-where s = x + fst r-}
          {-n = 1 + snd r-}
          {-r = sommeEtCompte xs-}


{- 
     Exemple de fonction main avec des entrées et des sorties 
-}
{-main :: IO()-}
{-main = do print "Entrer une liste de valeur : "-}
          {-cs <- getLine-}
          {-print "La moyenne des valeurs est : "-}
          {-let xs = read cs -}
            {-print $ moyenne cs-}

{- Commentaire : notez l'omission du 'in' dans le code. Celui-ci peut-être mis après 
                 le 'let' mais nécessite (au moins) un espace ou une tabulation pour 
                 marquer qu'il ne fait pas partie du bloc du 'do'. 
                 En général, il est donc omis. Voici la syntaxe qui serait valide. 
-}
{- 
main :: IO() 
main = do print "Entrer une liste de valeur : "
          cs <- getLine
          print "La moyenne des valeurs est : "
          let xs = read cs 
            in print $ moyenne cs
-}

