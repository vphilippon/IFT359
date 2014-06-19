{-
 -Fichier du premier laboratoire IFT359
 -du 2 mai 2014
 -}

-- Définition de x
x = 2

-- Définition d'un fonction
f x y = sqrt(x**2 + y**2)

-- Définition d'une fonction à l'aide de if
g :: Integer -> Integer
g n = if(n > 0)
      then if(n > 2)
           then 3
           else 1
      else 2

-- Définition de la même fonction avec pattern-matching
g' :: Integer -> Integer
g' n | n > 2 = 3
g' n | n > 0 = 1
g' n | otherwise = 2

-- Définition simple de la distance
d' :: (Double, Double) -> (Double, Double) -> Double
d' (xA, yA) (xB, yB) = sqrt( (yB -yA)**2 + (xB - xA)**2 )

-- Définition avec where
distance :: (Double, Double) -> (Double, Double) -> Double
distance (xA, yA) (xB, yB) = sqrt(dy**2 + dx**2)
    where dy = yB - yA
          dx = xB - xA

-- Définition avec let
distance' :: (Double, Double) -> (Double, Double) -> Double
distance' (xA, yA) (xB, yB) = let dy = yB - yA
                                  dx = xB - xA
                              in sqrt(dy**2 + dx**2)

-- Def factorielle
fact :: Integer -> Integer
fact n = if(n == 0)
         then 1
         else n * fact(n-1)

fact' :: Integer -> Integer
fact' 0 = 1
fact' n = n * fact'(n-1)
