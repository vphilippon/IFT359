{-| TP4 du cours IFT359 à remettre le 3 juillet 2014

    Remis par : Vincent Philippon - 12 098 838
-}
module TP4 
    (
    -- * Question 1 : un monoid 
    -- $question1
      Min (..)
    , minimum 
    -- * Question 2 : instances de classes pour un arbre binaire
    -- $question2
    , BTree (..) 
    , root
    , leaf
    , node
    , left
    , right
    , isLeaf
    , isNode
    , btree
    -- * Question 3 : Structure de file
    -- $question3
    , File (..)
    , vide
    , estVide
    , premier
    , reste
    , (<+)
    , (<+<)
    , renverse
    , taille 
    ) where

import Control.Applicative
import Data.List (foldl')
import Data.Monoid 
import qualified Prelude (minimum) 
import Prelude hiding (minimum)
-- ces deux lignes font que l'appel à la fonction @minimum@ du 'Prelude', 
-- se fait par 'Prelude.minimum'. De cette façon, l'appel à lafonction 
-- 'minimum' définie ici se fait sans ajout du préfixe @TP4@

{- $question1
   Monoid des minimums 

   Implémenter les instances de 'Functor' et de 'Applicative' pour le monoid 'Min'. 

   On rappelle que les propriétés suivantes doivent être vraies :
   
   >> -- Functor
   >> fmap id  ==  id
   >> fmap (f . g)  ==  fmap f . fmap g

   >> -- Applicative
   >> pure id <*> v = v
   >> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   >> pure f <*> pure x = pure (f x)
   >> u <*> pure y = pure ($ y) <*> u

   >> -- Monoid 
   >> mappend mempty x = x
   >> mappend x mempty = x
   >> mappend x (mappend y z) = mappend (mappend x y) z
   >> mconcat = 'foldr' mappend mempty
-}
newtype Min a = Min { getMin :: a }

instance Functor Min where
    fmap  = undefined 

instance Applicative Min where
    pure  = undefined 
    (<*>) = undefined 

instance (Bounded a, Ord a) => Monoid (Min a) where 
    mempty = Min maxBound
    mappend = liftA2 min 

minimum :: (Bounded a, Ord a) => [a] -> a 
minimum = getMin . mconcat . map Min 

{- $question2
   Avec la définition de la structure de donnée algébrique 'ArbreBinaire' qui vous est donnée, 
   instancier les classes 
   
    * 'Functor', c'est-à-dire la méthode 'fmap';

    * 'Applicative', c'est-à-dire les méthodes 'pure' et '(<*>)';

    * 'Monad', c'est-à-dire la méthode '(>>=)' (la méthode 'pure' correspond à la méthode 'pure'
      de la classe 'Applicative';
      
   On rappelle que les propriétés suivantes doivent être vraies :
   
   >> -- Functor
   >> fmap id  ==  id
   >> fmap (f . g)  ==  fmap f . fmap g

   >> -- Applicative
   >> pure id <*> v = v
   >> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   >> pure f <*> pure x = pure (f x)
   >> u <*> pure y = pure ($ y) <*> u

   >> -- Monad 
   >> return a >>= k  ==  k a
   >> m >>= return  ==  m
   >> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
   >> fmap f xs  ==  xs >>= return . f
-}

data BTree b a = Leaf a | Node b (BTree b a) (BTree b a)
    deriving (Show, Read, Eq, Ord)

root :: BTree b a -> Either b a
root = undefined

leaf :: BTree b a -> a
leaf = undefined 

node :: BTree b a -> b
node = undefined

left :: BTree b a -> BTree b a 
left = undefined

right :: BTree b a -> BTree b a 
right = undefined

isLeaf :: BTree b a -> Bool
isLeaf = undefined

isNode :: BTree b a -> Bool
isNode = undefined

btree :: (b -> c) -> (a -> c) -> BTree b a -> c 
btree = undefined 

instance Functor (BTree b) where
    fmap = undefined

instance Applicative (BTree b) where 
    pure = undefined
    f <*> x  = undefined 

instance Monad (BTree b) where 
    return = pure 
    m >>= k = undefined 

{- $question3
   Implémenter les fonctions utilisant la structure de donnée algébrique 'File'.
   Cette structure implémente une file de données, c'est-à-dire une structure
   de donnée du type FIFO (premier entrée, premier sorti). L'utilisation d'une 
   liste est la façon la plus simple et la plus évidente pour représenter cette
   structure. 

   Implémenter ensuite les fonctions indiquées dans la suite pour cette structure. 
   Pour chaque fonction, rajouter un commentaire indiquant la complexité temporelle
   de la fonction. La complexité des opérations sur les listes sont indiqués dans 
-}

data File a = File -- à faire 
    deriving (Eq, Ord, Show, Read) 

{- | @'vide'@ est la file vide -}
vide :: File a
vide = undefined

{- | @'estVide' xs@ retourne 'True' si la file @xs@ est vide; 'False' sinon. 
   On a les propriétés suivantes :

   >> estVide vide = True
   >> estVide $ ajoute xs x = False 
-}
estVide :: File a -> Bool
estVide = undefined 

{- | @'premier' xs@ retourne le premier élément en tête de la liste @xs@.
   Si @xs@ est vide, @'premier' xs@ retourne une erreur.
   On a les propriétés suivantes :

   >> premier $ vide = error "premier : file vide" 
-}
premier :: File a -> a
premier = undefined

{- | @'reste' xs@ retourne la file @xs@ privé de son premier élément.
   Si @xs@ est vide, @'reste' xs@ retourne une erreur.
   On a les propriétés suivantes :

   >> reste $ vide = error "reste : file vide" 
-}
reste :: File a -> File a
reste = undefined

{- | @xs '<+' x@ ajoute l'élément @x@ à la fin de la file @xs@ 
   On a les propriétés suivantes :

   >> premier $ vide <+ x = x
   >> reste $ vide <+ x = vide
-}
(<+) :: File a -> a -> File a
(<+) = undefined 

{- | @xs '<+<' ys@ retourne la File constituée des éléments de @xs@ suivis 
   de ceux de @ys@. 
   On a les propriétés suivantes :

   >> vide <+< xs = xs
   >> xs <+< vide = xs
   >> premier $ xs <+< ys = premier xs
   >> reste $ xs <+< ys = reste xs <+< ys 
   >> (xs <+< ys) <+ z = xs <+< (ys <+ z) 
-}
(<+<) :: File a -> File a -> File a
(<+<) = undefined 

{- | @'renverse' xs@ retourne la file des éléments de @xs@ disposés dans le 
   sens inverse de ceux de @xs@
   On a les propriétés suivantes :

   >> renverse vide = vide
   >> premier $ renverse $ xs <+ x = x  
   >> reste $ renverse $ xs <+ x = renverse xs 
   >> renverse $ xs <+< ys = renverse ys <+< renverse xs
-}
renverse :: File a -> File a
renverse = undefined 

{- | @'taille' xs@ retourne la taille de la file @xs@ 
   On a les propriétés suivantes :

   >> taille vide = 0
   >> taille $ xs <+ x = taille xs + 1
   >> taille $ reste xs = taille xs - 1
   >> taille $ xs <+< ys = taille xs + taille ys 
   >> taille $ renverse xs = taille xs 
-}
taille :: File a -> Int 
taille = undefined 

