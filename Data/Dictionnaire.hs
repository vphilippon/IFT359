module Data.Dictionnaire where

type Dictionnaire clés valeurs = [(clés,valeurs)]

{- Recherche -}

{- | @'ajoute' x xs@ retourne la liste @xs@ dans laquelle @x@ 
     a été ajoutée, s'il n'y était pas déjà 
-} 
ajoute :: Eq a => a -> [a] -> [a] 
ajoute x' [] = [x']
ajoute x' xs'@(x:xs) | x == x'   = xs'
ajoute x' (x:xs)     | otherwise = x : ajoute x' xs  

{- | @'valeurs' d@ retourne la liste des valeurs disponibles 
     en éliminant les doublons. 
-}
valeurs :: Eq v => Dictionnaire c v -> [v]
valeurs [] = []
valeurs ((_,v):dico) = ajoute v $ valeurs dico

{- | @'clé' d@ retourne la liste des clés disponibles 
     en éliminant les doublons. 
-}
clés :: Eq c => Dictionnaire c v -> [c]
clés [] = []
clés ((c,_):dico) = ajoute c $ clés dico 

{- | @'chercher' d c@ retourne la valeur de @v@ 
     tel que @(c,v)@ soit la première association
     trouvée dans le dictionnaire @d@, c'est-à-dire
     la dernière ajoutée. 
-}
chercher :: Eq c => Dictionnaire c v -> c -> Maybe v 
chercher [] _                        = Nothing
chercher ((c,v):dico) c' | c == c'   = Just v
chercher (_:dico) c'     | otherwise = chercher dico c'

{- | @'chercherTous' d c@ retourne toutes les valeurs @v@ 
     tel que @(c,v)@ soit une association du dictionnaire @d@.
-}
chercherTous :: Eq c => Dictionnaire c v -> c -> [v]
chercherTous dico c = map snd $ filter ((c==).fst) dico 

{- 
-- Alternative utilisant le fait que les listes ont des structures de monade
chercherTous :: Eq c => Dictionnaire c v -> c -> [v]
chercherTous dico c = do (c',v') <- dico 
                         if c == c' then [v'] else []  
-}

{- | @'insérer' d c v@ retourne le dictionnaire @d@ dans lequel
     la première association contenant la clé @c@ est modifiée
     pour @(c,v)@. -}
insérer :: Eq c => Dictionnaire c v -> c -> v -> Dictionnaire c v 
insérer [] c v = [(c,v)]
insérer ((c,_):dico) c' v'  | c == c'   = (c',v') : dico
insérer (couple:dico) c' v' | otherwise = couple : insérer dico c' v' 

