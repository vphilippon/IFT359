{- | TP3 IFT359 Été 2014 à remettre le 12 juin 2014 par turnin

    Remis par : Vincent Philippon - 12 098 838
-}
module TP3 ( 
           -- * Types
             Address
           , Seed
           , Password
           , Dictionary
           , Key
           -- * Question 1
           -- $question1
           , dictionary 
           , hashWith 
           -- * Question 2
           -- $question2
           , foldI
           , racine
           -- * Question 3 
           -- $question3
           , combiner  
           ) where  

import Data.Char (ord, chr) 
import Data.Monoid

type Address = String
type Seed = String
type Password = String 
type Dictionary = String
type Key = Int

{- QUESTION 1 -}
{- $question1
   = Génération de mot de passe par hashage

   On veut obtenir un générateur de mot de passe qui fonctionne en générant 
   une suite de chiffres @ds@ qui serviront à obtenir les lettres correspondantes 
   dans un dictionnaire passé en paramètre. La suite @ds@ est obtenue en utilisant 
   les valeurs ASCII des lettres de l'adresse où le mot de passe sera utilisée 
   et les valeurs ASCII des lettres d'un mot à retenir. Ceci permet de ne retenir
   qu'un mot quelque soit le site pour lequel on veut produire un mot de passe. 

   La procédure est la suivante :
   
   - On remplace chaque lettre de l'adresse et du mot générateur par
     les valeurs ASCII les représentants.

   - Chaque nombre @x@ du mot générateur est remplacé par @h x y@
     où @y@ est le nombre de l'adresse placée à la même position que
     @x@ (on agrandit l'adresse au besoin, en la répetant autant que
     nécessaire). Avec @n@ la longueur du mot générateur et @m@ est 
     la longueur du dictionnaire, on définie @h@ par

     > h x y = (x * y + n) `mod` m 

   - On substitue à chaque nombre le caractère correspondant du dictionnaire.
-}

dictionary :: Dictionary 
dictionary = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['!','?','_','@']

hashWith :: Address -> Dictionary -> Seed -> Password 
hashWith addr dict seed = map (dict !!) ds
    where ds         = zipWith h seedASCII addrASCII
          h x y      = (x * y + n) `mod` m
          n          = length seed
          m          = length dict
          seedASCII  = map ord seed
          addrASCII  = map ord $ cycle addr

{- QUESTION 2 -}
{- $question2
   = Calcul de la racine carrée par la méthode d'Héron de Babylone

    À l'aide de l'itérateur 'foldI' calculer la racine carrée d'un nombre 'n'
    passé en paramètre avec une précision 'p'. 
   
    Pour se faire, vous devez implémenter les fonctions 'gen', 'dx', 'test',
    'correctif' et la liste 'solutions'. Ces éléments vous permettront de 
    produire la fonction 'racine' qui calcule la racine carrée. 

    * La fonction @gen next seed@ crée une liste infinie commençant à @seed@
      et composée des applications de @next@ sur @seed@. C'est-à-dire

      >>> :t gen
      gen :: (a -> a) -> a -> [a]  
      >>> gen next seed
      [seed, next seed, next next seed, ... ]

    * La fonction @dx@ calcule la différence entre le nombre @n@ dont
      on veut trouver la racine carrée et l'argument de @dx@ mis au carré. 
      On retrouve ce calcul dans deux autres fonctions définies ici.

      >>> :t dx 
      dx :: Double -> Double 
      >>> dx 2   -- si n = 4
      0.0
      >>> dx 1   -- si n = 4
      3.0
      >>> dx 2.5 -- si n = 4
      -2.25

    * La fonction @test@ évalue si son argument est acceptable comme approximation.
      Cette approximation est, bien entendu, dépendante de @n@, le nombre dont on 
      veut évaluer la racine carrée, et @p@, la précision recherchée. Le test 
      @test x@ est valide si la distance entre @n@ et @x@ au carré est inférieur
      strictement à la précision recherchée. 

      >>> :t test 
      test :: Double -> Bool

    * La fonction @correctif@ calcule la modification à ajouter à une solution
      si celle-ci ne passe pas le test précédent. Cette fonction sert à implémenter
      la liste infinie @solutions@. La valeur du correctif pour une solution @x@ dépend 
      du nombre @n@ dont on recherche la racine carrée et est égal à la différence 
      entre @n@ et @x@ au carré, divisée par deux @x@. 

      >>> :t correctif 
      correctif :: Double -> Double

    * La liste @solutions@ donne la liste des solutions possibles. C'est une
      liste infinie créée à partir de @gen@, de l'estimation grossière @e@ et de 
      @correctif@. 

      >>> :t solutions 
      solutions :: [Double]
      >>> take 3 solutions -- pour n = 2
      [1.5,1.4166666666666667,1.4142156862745099] 

    L'idée est d'utilisée 'foldI' pour générer une boucle qui s'arrêtera lorsque
    la précision sera suffisante (ce qui est évalué par @test@). Lorsque ce n'est pas
    le cas, on continue l'évaluation en testant la solution suivante (dans @solutions@).
    Dans ce cas, il n'y a jamais de terminaison par liste vide. La valeur de l'élément
    initial dans 'foldI' est donc non pertinent. On pourra utiliser 'undefined'. 
-}

{- | @'foldI' p i op e xs@ réduit la liste @xs@ à l'aide de l'opérateur @op@
    en partant de la valeur @e@, (c'est-à-dire calcule @'foldr' op e xs@),
    en ajoutant la possibilité d'une interruption dans le cas où @p $ 'head' xs@.
    Dans ce cas, le retour est @i $ 'head' xs@. L'avantage de 'foldI' est de 
    pouvoir effectuer une réduction dans le cas d'une liste infinie en définissant
    un critère de terminaison. Par exemple,

    >>> foldI (>10) (*2) (+) 0 [1..] 
    77
    >>> let zip [1..] [a..] in 
-}
foldI :: (a -> Bool)    -- ^ le test établissant l'interruption aaa 
      -> (a -> b)       -- ^ la fonction de retour dans le cas d'une interruption
      -> (a -> b -> b)  -- ^ un opérateur binaire (idem à 'foldr')
      -> b              -- ^ l'élément initial (idem à 'foldr')
      -> [a]            -- ^ la liste à réduire (idem à 'foldr')
      -> b   
foldI p i op e []                 = e
foldI p i op e (x:xs) | p x       = i x
foldI p i op e (x:xs) | otherwise = x `op` foldI p i op e xs 

{- | @'racine' n p estimation@ calcule la racine carrée de @n@ tel que
     la distance entre @n@ et le carré du résultat est
     inférieur à @p@. Le paramètre @estimation@ est une estimation
     grossière utilisée pour démarrer le calcul. Par exemple, pour 
     calculer la racine carrée de 2 à 1e-5 près, on pourra utiliser

     >>> racine 2 1e-5 1.5  
     1.4142156862745099 
     >>> racine 2 1e-5 1.5 ** 2
     2.000006007304883
     >>> racine 2 1e-12 1.5
     1.4142135623730951
     >>> racine 2 1e-12 1.5 ** 2
     2.0000000000000004

     Cette méthode de calcul, appelée la méthode de Héron, a une 
     vitesse de convergence quadratique. 
-}
racine :: Double -- ^ le nombre @n@ dont on cherche la racine carrée
       -> Double -- ^ la précision @p@ du calcul
       -> Double -- ^ l'estimation grossière @e@ initiale du calcul 
       -> Double 
racine n p e = foldI test id (flip const) undefined solutions
    where -- gen :: (a -> a) -> a -> [a] 
          gen next seed = seed : gen next (next seed)
          -- dx :: Double -> Double 
          dx = (n-) . (**2)
          -- test :: Double -> Bool
          test = (<p) . abs . dx
          -- correctif :: Double -> Double
          correctif x = dx x / (2*x)
          -- solutions :: [Double]
          solutions = gen (\ x -> x + correctif x) e

{- QUESTION 3 -}
{- $question3
= Création d'un combinateur

On demande dans cette question de trouver un combinateur 'combiner' à partir des trois 
exemples @decodeMessage@, @traduction@ et @developpe@, puis de les implémenter en 
utilisant le combinateur. On suppose qu'on dispose de quatre fonctions @decodeMorceau@, 
@traductionMot@, @developpeSymbole@ et @combinerAvec@ dont les types sont donnés par la suite. 
On suppose qu'on dispose aussi d'un type abstrait (c'est-à-dire encapsulé) @Circuit@.

> decodeMorceau :: Integer -> (String, Integer) 
> decodeMorceau = ...
>
> decode :: [Integer] -> (String, Integer)
> decode [] = ("", 0)
> decode (i:is) = let (message, checksum) = decode is 
>                     (morceaux, checksum') = decodeMorceau i
>                 in (morceaux ++ message, checksum' + checksum)

> traductionMot :: String -> ([String], String)
> traductionMot = ...
>
> traduction :: [String] -> ([String], String) 
> traduction [] = (0, "")
> traduction (mot:mots) = let (t, log) = traduction mots
>                             (t', log') = traductionMot mot
>                         in (t' ++ t, log' ++ log) 

> newtype Circuit = ...
>
> developpeSymbole :: Char -> ([Circuit], Maybe Circuit)
> developpeSymbole = ...
>
> combinerAvec :: Maybe Circuit -> Maybe Circuit -> Maybe Circuit
> combinerAvec = ...
>
> developpe :: String -> ([Circuit], Maybe Circuit)
> developpe "" = ([], Nothing)
> developpe (c:cs) = let (circuits, d) = developpe cs
>                        (circuits', d') = developpeSymbole c
>                    in (circuits' ++ circuits, d' `combinerAvec` d) 
-}

{- | 'combiner' est le combinateur à implémenter et dont
      vous devez fournir le type
-} 
{-combiner :: (Monoid c) => (a -> ([b], c)) -> [a] -> ([b], c)-}
{-combiner _ [] = ([], mempty)-}
{-combiner f (x:xs) = let (u, v) = combiner f xs-}
                        {-(u', v') = f x-}
                    {-in (u' ++ u, v' `mappend` v)-}
combiner :: (a -> ([b], c)) -> (c -> c -> c ) -> c -> [a] -> ([b], c)
combiner _ _ e [] = ([], e)
combiner f comb e (x:xs) = let (u, v) = combiner f comb e xs
                               (u', v') = f x
                           in (u' ++ u, v' `comb` v)

{- IMPLÉMENTER ICI LES FONCTIONS AVEC combiner -}
{-
decode = combiner decodeMorceau (+) 0
traduction = combiner traductionMot (++) ""
developpe = combiner developpeSymbole combinerAvec Nothing
-}
