module Data.Point
    ( Coordonnee
    , Point
    , abscisse
    , ordonnee
    , creer
    )
    where

type Coordonnee = Integer 

type Point = (Coordonnee, Coordonnee)

abscisse :: Point -> Coordonnee
abscisse (x,_) = x

ordonnee :: Point -> Coordonnee
ordonnee (_,y) = y

creer :: Coordonnee -> Coordonnee -> Point
creer = (,)

