import qualified Control.Applicative

fs <*> xs = foldr (++) [] $ map apply fs
    where apply g = map g xs



