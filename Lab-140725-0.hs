-- Code repris de http://learnyouahaskell.com/zippers
-- Les Zippers est une structure de donnéees imaginées par Gérard Huet (http://pauillac.inria.fr/~huet/)

module Lab140725 where

data Arbre a = Noeud a (Arbre a) (Arbre a) | Vide
    deriving (Show, Eq)

data Direction = G | D
    deriving (Show, Eq)

type Directions = [Direction]

-- exemple
arbre = Noeud 'I' 
              (Noeud 'F' 
                     (Noeud 'T' Vide Vide)
                     Vide
              )
              (Noeud 'G' 
                     (Noeud 'L' Vide Vide)
                     (Noeud 'R' Vide Vide)
              )

-- Horrible code !
changerArbre :: Arbre Char -> Arbre Char
changerArbre (Noeud x g1 (Noeud y g2 (Noeud _ g3 d3))) = Noeud x g1 $ Noeud y g2 $ Noeud 'E' g3 d3

{- Première approche -} 
changerNoeud :: a -> Directions -> Arbre a -> Arbre a
changerNoeud x = modifier
    where --modifier _ Vide = error "changerNoeud : arbre vide"
          modifier [] (Noeud _ g d) = Noeud x g d
          modifier (G:ds) (Noeud x g d) = Noeud x (modifier ds g) d
          modifier (D:ds) (Noeud x g d) = Noeud x g (modifier ds d)

valeur :: Directions -> Arbre a -> a
valeur [] (Noeud x _ _) = x
valeur (G:ds) (Noeud _ g _) = valeur ds g
valeur (D:ds) (Noeud _ _ d) = valeur ds d

test1 = do print $ valeur [D,D] arbre
           let arbre' = changerNoeud 'E' [D,D] arbre
           print $ arbre'
           print $ valeur [D,D] arbre'

-- VP ADD
type Miettes = [Direction]

gauche :: (Arbre a, Miettes) -> (Arbre a, Miettes)  
gauche (Noeud _ g _, ms) = (g, G:ms)  

droite :: (Arbre a, Miettes) -> (Arbre a, Miettes)  
droite (Noeud _ _ d, ms) = (d, D:ms)  

-- Move around the tree
x -: f = f x 

