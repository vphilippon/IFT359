{-| TP5 du cours IFT359 à remettre le 25 juillet 2014

    Remis par : Vincent Philippon - 12 098 838

    Type de remise : 4) 
    --> Fait "tout".
    ==========================================
-}

module TP5 
    ( 
    -- * Éléments de l'analyseur syntaxique

    -- | Les éléments de cette section peuvent être modifiées.
    --   Ajouter un commentaire en tête de la fonction pour 
    --   qu'elle soit aisément identifiable 
    
      parserMagicNumber --Ajout
    , parserEntete --Ajout
    , parserTrame --Ajout
    , validRange --Ajout
    , spaceOrComment --Ajout
    , comment --Ajout

    -- ** Type et instanciation
    , Parser (..)
    
    -- ** Primitives
    , (+++)
    , failure
    , item
    , sat
    , guard
    
    -- ** Combinateurs
    , many
    , many1
    , sepby
    , sepby1  
    
    -- ** Combinateurs lexicaux
    , space
    , token 

    -- ** Analyseurs (Parser) élémentaires sur les caractères 
    , char       
    , string
    , lower      
    , upper      
    , letter     
    , alphanum   
    , symb       
    , ident 

    -- ** Analyseurs (Parser) élémentaires numériques 
    , digit      
    , nat
    , natural
    , int
    , integer

    -- * Question
    -- $Q1
    , result
    , parseFromFile 
    , fromFile 
    , fromString
    ) where 

import Control.Monad
import Data.Char 
import Data.List (foldl') 

import Data.Image 

import Data.Entete (Entete (..))
import qualified Data.Entete as Entete 

import Data.Trame (Trame)
import qualified Data.Trame as Trame 

import Data.Pixel (Pixel (..))
import qualified Data.Pixel as Pixel

import Data.Point (Point,Coordonnee)
import qualified Data.Point as Point 

import Data.Couleur 

{- $Q1
    implémenter un analyseur syntaxique (un parser) qui permet de lire 
    une image au format PPM à partir d'un fichier.
    Les primitives utilisées pour l'analyse syntaxique vues en laboratoire 
    sont disponibles dans ce fichier.

    Vous avez quatre implémentations possibles.  Vous devez en choisir une :

    1) implémenter un analyseur syntaxique qui permet d'analyser une chaîne
        de caractère contenant le contenu d'un fichier au format PPM, sans 
        commentaire. Toutes erreurs dans le fichier entraîne l'échec de l'analyse,
        dont notamment les erreurs suivantes (6 points) : 

        - la largeur ou la hauteur de l'image est incorrecte (trop ou pas assez de 
          valeurs dans la trame)
        
        - la trame est incorrecte car elle contient d'autres valeurs que des entiers 
        
        - la trame est incorrecte car des valeurs de la trame sont supérieures à la 
          valeur maximale ou inférieure à 0

    2) implémenter l'analyseur syntaxique de la question 1 en passant par dessus
        les erreurs spécifiques mentionnées dans la question 1 qui sont rencontrées, 
        c'est-à-dire en continuant l'analyse syntaxique et en traitant l'erreur de la
        façon suivante (8 points) :

        - si la largeur ou la hauteur est trop petite, on ignore les valeurs
          surnuméraires de la trame

        - si la largeur ou la hauteur est trop grande pour le nombre de valeurs
          dans la trame, on rempli avec des 0 pour les valeurs manquantes
        
        - les valeurs négatives sont traitées comme des 0, les valeurs supérieurs
          à la valeur maximale sont traitées comme la valeur maximale
        
        - un caractère qui n'est pas un chiffre dans la trame est considéré comme
          un espace (il est donc ignoré)

    3) implémenteur l'analyseur syntaxique de la question 1 en traitant les 
        commentaires dans le texte du fichier. (8 points)

    4) implémenter l'analyseur de la question 1 avec les critères de la question
        2 et de la question 3  (10 points)

    Le format d'une image PPM est décrite à l'adresse <http://netpbm.sourceforge.net/doc/ppm.html>
-}

data Parser a = P { parse :: String -> Maybe (String,a) }


instance Monad Parser where
    return x = P (\ cs -> Just (cs,x))
    p >>= g = P (\ cs -> case (parse p cs) of
                           Nothing -> Nothing
                           Just (cs',x) -> parse (g x) cs')

instance MonadPlus Parser where 
    mzero = P (\ cs -> Nothing)
    p1 `mplus` p2 = P (\ cs -> case (parse p1 cs) of
                                 Nothing -> parse p2 cs
                                 r@_     -> r) 
-- MonadPlus définie 'Control.Monad.guard'

{- ---------------------------
    Primitives
   --------------------------- -}

(+++) :: Parser a -> Parser a -> Parser a
(+++) = mplus

failure :: Parser a
failure = mzero  

item :: Parser Char
item = P (\cs -> case cs of
                   ""       -> Nothing
                   (c':cs') -> Just (cs',c'))

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else failure

{- ---------------------------
    Combinateurs
   --------------------------- -}

many            :: Parser a -> Parser [a]
many p           = many1 p +++ return []

many1           :: Parser a -> Parser [a]
many1 p          = do a <- p
                      as <- many p
                      return (a:as)

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do a <- p
                      as <- many (sep >> p) 
                      return (a:as)

{- ---------------------------
    Combinateurs lexicaux 
   --------------------------- -}

space           :: Parser String
space            = many (sat isSpace)

token           :: Parser a -> Parser a
token p          = do a <- p
                      space
                      return a

{- ---------------------------
    Parsers élémentaires (Char et String)
   --------------------------- -}

char            :: Char -> Parser Char
char c           = sat (c ==)

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do char c
                      string cs
                      return (c:cs)

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = sat isAlpha

alphanum        :: Parser Char
alphanum         = sat isAlphaNum

symb            :: String -> Parser String
symb cs          = token (string cs)

ident           :: [String] -> Parser String
ident css        = do cs <- token $ oneWord 
                      guard (not (elem cs css))
                      return cs
    where oneWord = many alphanum

{- ---------------------------
    Parsers élémentaires (Int)
   --------------------------- -}

digit           :: Parser Int
digit            = do c <- sat isDigit
                      return (ord c - ord '0')

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit >>= prefixToDigits  
    where prefixToDigits n = do m <- digit
                                prefixToDigits $ n*10 + m
                             +++ return n 

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do char '-'
                      n <- natural
                      return (-n)
                   +++ nat


{- ============
   question 
   ============ -} 

result :: Maybe (String, a) -> Maybe a
result m = do (cs, x) <- m
              if (cs == "") then (return x) else Nothing 

parseFromFile :: Parser a -> String -> IO (Maybe a)
parseFromFile p fname = readFile fname >>= return . result . parse p 

fromFile :: String -> IO (Maybe Image)
fromFile = parseFromFile parserPPM 

fromString :: String -> Maybe Image
fromString = result . parse parserPPM  

parserPPM :: Parser Image
parserPPM = do parserMagicNumber
               e <- parserEntete
               t <- parserTrame (hauteur e) (largeur e)
               return $ creer e t  -- Image.creer

-- !!! FONCTIONS AJOUTÉES !!!
-- ==========================

-- Ajout: parserMagicNumber
parserMagicNumber :: Parser String
parserMagicNumber = do many comment
                       string "P3"

-- Ajout: parserEntete
parserEntete :: Parser Entete
parserEntete = do many1 spaceOrComment
                  w <- nat
                  many spaceOrComment
                  h <- nat
                  many spaceOrComment
                  return $ Entete.creer (toInteger w) (toInteger h)

--Ajout parserTrame
parserTrame :: Integer -> Integer -> Parser Trame
parserTrame w h = do max <- nat
                     many spaceOrComment
                     valeurs <- many $ validRange max
                     let couleurs = faireCouleur (valeurs ++ [0,0..]) (w * h) []
                     let points = [Point.creer x y | x <- [0..w-1], y <- [0..h-1]]
                     let pixels = take (fromIntegral $ w * h) (zipWith Pixel.creer points couleurs)
                     return $ Trame.creer pixels
                       where faireCouleur _ 0 acc          = acc
                             faireCouleur (r:v:b:cs) n acc = faireCouleur cs (n-1) (acc ++ [(RGB r v b)])

--Ajout validRange
validRange :: Int -> Parser Int
validRange max = do v <- int
                    many spaceOrComment
                    return $ correction v
                 +++ 
                 do sat (not . isDigit) 
                    v <- validRange max
                    return $ v
                        where correction val
                                  | val <= 0  = 0
                                  | val > max = max
                                  | otherwise = val


--Ajout spaceOrComment
spaceOrComment = sat isSpace +++ comment

--Ajout comment
comment :: Parser Char
comment = do space
             char '#'
             many ( sat( \c -> c /= '\n') )
             char '\n'

