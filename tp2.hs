{- TP2 IFT359 Été 2014 à remettre le 23 mai 2014 par turnin

   Noms des membres de l'équipe et matricule :
   - Vincent Philippon  12 098 838
-}

module TP2 ( 
           -- * Nouveaux types
             Position
           , Labyrinthe
           , Chemin
           -- * Question 1
           -- $question1
           , exempleDeLabyrinthe
           , horsLabyrinthe 
           , estUnMur
           , estUneRue
           , estMarquee
           , marque
           , plusCourtChemin
           -- * Question 2
           -- $question2
           , partition
           , partition'
           -- * Question 3 
           -- $question3
           , (<?)  
           -- * Question 4 
           -- $question4
           , pythagoriciens  
           -- * Question 5 
           -- $question5
           , position
           , positions
           ) where  

{- QUESTION 1 -}
{- $question1 
   Le plus court chemin dans un labyrinthe :
   Implémenter les fonctions suivantes pour résoudre
   le problème du plus court chemin vu en classe 
-}

type Position = (Int,Int)
type Labyrinthe = [[Integer]] 
type Chemin = [Position] 

exempleDeLabyrinthe :: Labyrinthe 
exempleDeLabyrinthe = [[ 0, 1, 1, 1, 1, 1, 1, 1 ],
                       [ 0, 0, 0, 0, 0, 1, 0, 1 ],
                       [ 0, 0, 1, 1, 1, 1, 1, 1 ]
                       -- la suite 
                       ]

-- | @'horsLabyrinthe' labyrinthe position@ indique si @position@ 
-- est en dehors de @labyrinthe@ 
horsLabyrinthe :: Labyrinthe -> Position -> Bool
horsLabyrinthe [] (_,_) = True
horsLabyrinthe (xs:_) (0,c) = parcoursLigne xs c 
    where parcoursLigne [] c = True
          parcoursLigne (_:xs) 0 = False
          parcoursLigne (_:xs) c = parcoursLigne xs (c-1)
horsLabyrinthe (_:xss) (l,c) = horsLabyrinthe xss (l-1,c) 

-- | @'estUnMur' labyrinthe position@ indique si la @position@ 
-- pointe sur un mur dans le @labyrinthe@
estUnMur :: Labyrinthe -> Position -> Bool
estUnMur (xs:_) (0,c) = estValeur xs c 0
estUnMur (_:xss) (l,c) = estUnMur xss (l-1,c)

-- | @'estUneRue' labyrinthe position@ indique si la @position@ 
-- pointe sur une rue dans le @labyrinthe@
estUneRue :: Labyrinthe -> Position -> Bool
estUneRue (xs:_) (0,c) = estValeur xs c 1
estUneRue (_:xss) (l,c) = estUneRue xss (l-1,c)

-- | @'estMarquee' labyrinthe position@ indique si la @position@ 
-- est marquée comme visitée dans le @labyrinthe@; 
-- cette fonction doit satisfaire que @'estMarquee' ('marque' l p) p = 'True'@ 
estMarquee :: Labyrinthe -> Position -> Bool
estMarquee (xs:_) (0,c) = estValeur xs c 2
estMarquee (_:xss) (l,c) = estMarquee xss (l-1,c)

-- Indique si l'élément à la position p est de valeur v
estValeur :: [Integer] -> Int -> Integer -> Bool
estValeur (x:_) 0 v = x == v
estValeur (_:xs) p v = estValeur xs (p-1) v

-- | @'marque' labyrinthe position@ 
-- marque la case à @position@ comme marquée ;
-- cette fonction doit satisfaire que @'estMarquee' ('marque' l p) p = 'True'@ 
marque :: Labyrinthe -> Position -> Labyrinthe 
marque (xs:xss) (0,c) = nouvelle:xss
    where nouvelle = [ if i == c then 2 else x | (x,i) <- liste]
          liste = (zip xs [0..n])
          n = length xs - 1
marque (xs:xss) (l,c) = xs:marque xss (l-1,c)


{-| @'plusCourtChemin' labyrinthe depart arrivee@  
    calcule le chemin le plus court dans @labyrinthe@ 
    allant de @depart@ à @arrivee@. 
    -}
plusCourtChemin :: Labyrinthe -> Position -> Position -> Maybe Chemin 
plusCourtChemin labyrinthe depart arrivee
    | horsLabyrinthe labyrinthe depart = Nothing
    | estUnMur labyrinthe depart       = Nothing
    | estMarquee labyrinthe depart     = Nothing
    | depart == arrivee                = Just [arrivee]
    | estUneRue labyrinthe depart = ajoute depart chemin
          where chemin = choisirDirection (epure [h, b, g, d])
                h = plusCourtChemin courrant (l-1,c) arrivee
                b = plusCourtChemin courrant (l+1,c) arrivee
                g = plusCourtChemin courrant (l,c-1) arrivee
                d = plusCourtChemin courrant (l,c+1) arrivee
                courrant = marque labyrinthe depart
                (l,c) = depart

ajoute :: Position -> Chemin -> Maybe Chemin
ajoute _ [] = Nothing
ajoute x xs = Just (x:xs)

epure :: [Maybe Chemin] -> [Chemin]
epure [] = []
epure (Nothing:xs) = epure xs
epure ((Just x):xs) = x:epure xs

choisirDirection :: [Chemin] -> Chemin
choisirDirection [] = []
choisirDirection (x:[]) = x
choisirDirection (x:xs) | x <? y    = x -- Utilisation du <? du #3
                        | otherwise = y 
    where y = choisirDirection xs


{- QUESTION 2 -}
{- $question2 
   La partition d'une liste en deux :
   Implémenter la fonction 'partition'' afin d'implémenter 'partition'
-}

{-| @'partition' ns pivot@ retourne un couple de deux listes contenant
    les éléments de @ns@ strictement plus petits que @pivot@ et les éléments
    de @ns@ plus grands ou égaux que @pivot@, dans l'ordre d'apparition dans @ns@. 
    Par exemple, @'partition' [3,10,5,8,9,15,20,3,51,42,8] 10@ doit retourner
    le couple @([3,5,8,9,3,8], [10,15,20,51,42])@ 
    -}
partition  :: [Integer] -> Integer -> ([Integer],[Integer])
partition ns pivot = partition' ns pivot [] []

{-| @'partition'' ns pivot infs sups@ retourne un couple de deux listes contenant
    les éléments de @ns@ strictement plus petits que @pivot@ et les éléments
    de @ns@ plus grands ou égaux que @pivot@, dans l'ordre d'apparition dans @ns@
    et placés à l'arrière, respectivement, des listes @infs@ et @sups@. 
    Par exemple, @'partition' [3,10,5,8,9,15,20,3,51,42,8] 10 [0] [1]@ doit retourner
    le couple @([0,3,5,8,9,3,8], [1,10,15,20,51,42])@  
    -}
partition' :: [Integer] -- ^ la liste à partitionner 
           -> Integer   -- ^ le pivot servant à la partition
           -> [Integer] -- ^ la liste permettant d'empiler les nombres inférieurs au pivot
           -> [Integer] -- ^ la liste permettant d'empiler les nombres supérieurs au pivot
           -> ([Integer],[Integer])
partition' [] _ infs sups = (infs,sups)
partition' (n:ns) pivot infs sups | pivot > n = partition' ns pivot (infs ++ [n]) sups 
partition' (n:ns) pivot infs sups | otherwise = partition' ns pivot infs (sups ++ [n])

{- QUESTION 3 -}
{- $question3
   Un ordre canonique sur les listes :
   Implémenter l'opérateur '<?' qui teste si deux listes sont placées dans l'ordre canonique.
-}

{-| @xs '<?' ys@ évalue si la liste @xs@ est placée avant @ys@ selon l'ordre canonique sur les listes.
    L'ordre canonique est défini comme suit: 
    
    * si les deux listes sont égales  @xs '<?' ys == 'False'@

    * si @xs@ est de taille inférieure à @ys@ alors @xs@ est plus petit que @ys@, 
      c'est-à-dire @xs '<?' ys == 'True'@

    * sinon si @ys@ est de taille inférieure à @ys@ alors @xs@ est plus grand que @xs@,
      c'est-à-dire que @xs '<?' ys == 'False'@

    * sinon, le premier élément de @xs@ qui diffère de celui de @ys@ détermine l'ordre

    Voici une liste d'exemple illustrant chaque cas

    >>> [0,1,2] <? [0,1,2] 
    False

    >>> [1,2] <? [0,1,2]
    True

    >>> [0,1,2,3] <? [1,2]
    False

    >>> [1,2,3,4,5,6] <? [1,2,3,5,5,6]
    True 

    >>> [1,2,3,4,5,6] <? [1,2,3,0,5,6]
    False
    -}
(<?) :: Ord a => [a] -> [a] -> Bool 
(<?) xs ys = ordreCanon xs ys False False

{-
 -ordreCanon l1 l2 differe plusPetit 
 - l1 : La liste 1
 - l2 : La liste 2
 - differe : Est-ce que le premier différent entre l1 et l2 a été rencontré?
 - plusPetit : Est-ce que l'élément de l1 est plus petit au premier différent?
 -
 - J'avais le gout de ne pas utiliser "length", le faire en 1 passe.
 - Pour le plaisir et l'apprentissage.
 -}
ordreCanon :: Ord a => [a] -> [a] -> Bool -> Bool -> Bool
ordreCanon [] (x:xs) _ _ = True
ordreCanon (x:xs) [] _ _ = False
ordreCanon [] [] _ plusPetit = plusPetit
ordreCanon (x:xs) (y:ys) False plusPetit | x < y     = ordreCanon xs ys True True 
                                         | x > y     = ordreCanon xs ys True False                                          
                                         | otherwise = ordreCanon xs ys False plusPetit 
ordreCanon (x:xs) (y:ys) True plusPetit = ordreCanon xs ys True plusPetit 


{- QUESTION 4 -}
{- $question4
    Les nombres pythagoriciens :
    Implémenter la fonction 'pythagoriciens' qui donne la liste des 
    nombres pythagoriciens inférieurs au paramètre de la fonction.
    L'implémentation doit être effectuée à l'aide du principe de compréhension.
-}

{-| @'pythagoriciens' n@ retourne la liste des triplets @(a,b,c)@ tels
    que @a**2 + b**2 == c**2@. Par exemple

    >>> pythagoriciens 10 
    [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
   -}
pythagoriciens :: Int -> [(Int,Int,Int)] 
pythagoriciens n = [ (a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]


{- QUESTION 5 -}
{- $question5 
    Fonction 'position' généralisée
    Implémenter la fonction 'positions' en utilisant
    les listes par compréhension et en s'inspirant de 
    l'implémentation de 'position', c'est-à-dire de 
    l'usage de 'zip'. 
-}

-- | @'position' x ys@ retourne la position de @x@ dans la liste @ys@
position :: Eq a => a -> [a] -> Int
position x ys = cherche (zip ys [0..n])
    where n = length ys - 1
          -- cherche :: [(a,Int)] -> Int  
          cherche ((y,n) : _) | x == y    = n
          cherche (_ : yns)   | otherwise = cherche yns 

-- | @'positions' x ys@ retourne la liste de toutes les positions de @x@ dans la liste @ys@
positions :: Eq a => a -> [a] -> [Int]
positions x ys = [ i | (y,i) <- liste, y == x]
    where liste = (zip ys [0..n])
          n = length ys - 1

