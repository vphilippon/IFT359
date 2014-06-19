module Lab140523 where 

import Control.Monad (liftM) 

-- retour sur la fonction integrale
type Integrale = Either String Double

integrale :: [Double]  -- la liste des valeurs de la fonction
          -> Double    -- la borne inférieure de l'intégrale
          -> Double    -- la borne supérieure de l'intégrale 
          -> Integrale  
integrale [] _ _           = Left "Pas de valeurs"
integrale _ a b | b < a    = Left "b < a"
integrale [_] a b | a /= b = Left "1 valeur et a /= b"
integrale valeurs a b = let delta = (b - a) / fromIntegral n
                            n = length valeurs
                            milieu x y = (x + y) / 2
                            paireValeurs = zip valeurs (tail valeurs)
                         in Right $ sum [ delta * milieu v1 v2 | (v1,v2) <- paireValeurs ]
                         {-$ remplace une grosse parenthèse à la suite.-}

                         {-Ça, c'est quadratique ( le !! doit parcourir à chaque fois...-}
                         {-in sum [ delta * milieu (valeurs!!i) (valeurs!!(i+1))  | i <- [0 .. n-2] ]-}

extraireMessage :: Integrale -> Maybe String
extraireMessage (Left s) = Just s
extraireMessage (Right _) = Nothing

result :: Integrale -> String
result (Left s) = "erreur" ++ s
result (Right d) = "le résultat est " ++ show d
{-Version plus courte-}
{-result i = either id show i-}


-- Implémentation du tri fusion en Haskell

division :: [a] -> ([a],[a]) 
division [] = ([],[])
division l@[x] = (l,[]) {- l@Something fait que l est un alias de Something, pas de reconstruction!-}
division (x:y:xs) = (x:l,y:r)
    where (l,r) = division xs

fusion :: Ord a => [a] -> [a] -> [a]
fusion xs [] = xs 
fusion [] ys = ys
fusion l1@(x:xs) l2@(y:ys)
    | x < y = x:fusion xs l2
    | otherwise = y:fusion l1 ys

tri :: Ord a => [a] -> [a] 
tri [] = []
tri [x] = [x]
tri xs = fusion ys' zs'
    where ys' = tri ys
          zs' = tri zs
          (ys, zs) = division xs

{- Fonction 'main' pour tester le calcul de l'intégrale -} 
main :: IO ()
main = do valeurs <- read `liftM` getLine
          a <- read `liftM` getLine
          b <- read `liftM` getLine
          putStrLn $ result $ integrale valeurs a b 


