module Lab140718 where 

import Data.Char 
import Data.Maybe (fromJust) 
import Data.List (foldl') 

data Expression = Multiplication Expression Expression 
                | Addition Expression Expression
                | Negation Expression 
                | Nombre Integer 
   deriving (Read, Show, Eq, Ord) 

{- ---------------------------
   Interpreteur 
   --------------------------- -}

calcule :: Expression -> Integer 
calcule (Nombre n) = n
calcule (Negation e) = - calcule e
calcule (Multiplication e1 e2) = calcule e1 * calcule e2
calcule (Addition e1 e2) = calcule e1 + calcule e2

fromString :: String -> Expression 
fromString = snd . fromJust . parse expr 

{- ============
   Type Parser  
   ============ -} 

data Parser a = P { parse :: String -> Maybe (String,a) }


instance Monad Parser where
    return x = P (\ cs -> Just (cs,x))
    p >>= g = P (\ cs -> case (parse p cs) of
                           Nothing -> Nothing
                           Just (cs',x) -> parse (g x) cs')

{- ---------------------------
    Primitives
   --------------------------- -}

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = P (\ cs -> case (parse p1 cs) of
                         Nothing -> parse p2 cs
                         r@_     -> r)

failure :: Parser a
failure = P (\cs -> Nothing) 

item :: Parser Char
item = P (\cs -> case cs of
                   ""       -> Nothing
                   (c':cs') -> Just (cs',c'))

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else failure

guard           :: Bool -> Parser ()
guard True      =  return ()
guard False     =  failure 

{- ---------------------------
    Combinateurs
   --------------------------- -}

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do char c
                      string cs
                      return (c:cs)

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
    Parsers élémentaires 
   --------------------------- -}

char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do c <- sat isDigit
                      return (ord c - ord '0')

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
ident css        = do cs <- token identifier
                      guard (not (elem cs css))
                      return cs

{- ============
   PARSERS 
   ============ -} 

identifier      :: Parser String
identifier       = do c <- lower
                      cs <- many alphanum 
                      return (c:cs)

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit >>= prefixToDigits  
                   where prefixToDigits n = do m <- digit
                                               prefixToDigits $ n*10 + m
                                            +++ return n 
{-
natural' = do ds <- many1 digit
              return $ foldl' (\ m n -> 10*m+n) 0 ds
-}

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do char '-'
                      n <- natural
                      return (-n)
                   +++ nat

{- ============
   Analyse syntaxique d'expression 
   ============ -} 

plus :: Parser Char
plus = char '+' 

fois :: Parser Char
fois = char '*' 

moins :: Parser Char
moins = char '-' 

lpar :: Parser Char
lpar = char '(' 

rpar :: Parser Char
rpar = char ')' 

{- ============
   Analyse syntaxique d'expression 
   ============ -} 


expr :: Parser Expression
expr = exprAdd

exprAdd :: Parser Expression
exprAdd = do m <- exprMult
             do token plus
                p <- exprAdd
                return $ Addition m p
              +++ return m

exprMult :: Parser Expression
exprMult = do a <- exprAt
              do token fois
                 m <- exprMult
                 return $ Multiplication a m
               +++ return a

exprAt :: Parser Expression 
exprAt = exprPar +++ exprNegation +++ exprNombre 

exprPar :: Parser Expression
exprPar = do token lpar
             e <- expr
             token rpar
             return e

exprNegation :: Parser Expression
exprNegation = do token moins 
                  e <- exprNombre 
                  return $ Negation e

exprNombre :: Parser Expression
exprNombre = nat >>= return . Nombre . fromIntegral  
                

