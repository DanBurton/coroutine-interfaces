import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad

import qualified Data.Map as Map

arrowPure :: (Arrow a) => z -> a x z
arrowPure z = arr (const z)

arrowAp :: (Arrow a) => a x (y -> z) -> a x y -> a x z
arrowAp af ax = uncurry ($) ^<< (af &&& ax)

data Map a b
  = PartialFunction b (a -> Maybe b)
  | TotalFunction (a -> b)

withDefault :: Ord a => b -> Map.Map a b -> Map a b
withDefault b m = PartialFunction b (\a -> Map.lookup a m)

toFunc :: Map a b -> a -> b
toFunc (TotalFunction f) a = f a
toFunc (PartialFunction def f) a = case f a of
  Nothing -> def
  Just b -> b

instance Category Map where
  id = arr id
  TotalFunction c1 . TotalFunction c2 = TotalFunction (c1 . c2)
  TotalFunction c1 . PartialFunction def c2 = PartialFunction (c1 def) ((Just . c1) <=< c2)
  PartialFunction def c1 . TotalFunction c2 = PartialFunction def (c1 <=< (Just . c2))
  PartialFunction def1 c1 . PartialFunction _ c2 = PartialFunction def1 (c1 <=< c2)

instance Arrow Map where
  arr = TotalFunction
  first f = TotalFunction (first $ toFunc f)

instance Functor (Map x) where
  fmap = (^<<)

instance Applicative (Map x) where
  pure = arrowPure
  (<*>) = arrowAp
