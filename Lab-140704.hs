
import Data.Tree
import Data.Traversable

import Control.Monad.State


numerote :: a -> State Int (Int,a)
numerote x = do i <- get
                put (i+1)
                return (i,x)
