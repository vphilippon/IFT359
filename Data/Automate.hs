module Data.Automate where 

import Control.Executable

data AutomateD etats etiquettes = AFD { nom :: String
                                      , transition :: etats -> etiquettes -> Maybe etats
                                      , depart :: etats
                                      , finaux :: etats -> Bool
                                      }

instance Show s => Executable (AutomateD s) where
    execute a e = let q0 = depart a
                      delta = transition a
                  in case delta q0 e of
                        Nothing -> Left  $ "Echec en " ++ show q0 
                        Just q1 -> Right $ AFD (nom a) delta q1 (finaux a) 


instance Show etats => Show (AutomateD etats etiquette) where
    show a = (nom a) ++ "(" ++ etatCourant ++ ")"
        where etatCourant = show $ depart a 

