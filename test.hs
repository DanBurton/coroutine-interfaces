{-# LANGUAGE Rank2Types, ConstraintKinds #-}

type IxMTrans t = forall i j m. Monad m => Monad (t i j m)

class (forall i j m. Monad m => Monad (t i j m)) => IxMFunctor t where
  map :: (Monad m, Monad m') => (forall x. m x -> m' x) -> t j k m a -> t j k m' a

{- error message

test.hs:5:1:
    Malformed predicate `forall i j (m :: * -> *).
                         Monad m =>
                         Monad (t i j m)'
    In the context: (forall i j (m :: * -> *).
                     Monad m =>
                     Monad (t i j m))
    While checking the super-classes of class `IxMFunctor'
    In the class declaration for `IxMFunctor'
Failed, modules loaded: none.

sadface -}