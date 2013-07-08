{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Util where

import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.MTrans as MT

lift0 = id
lift1 = lift . lift0
lift2 = lift . lift1
lift3 = lift . lift2
lift4 = lift . lift3
lift5 = lift . lift4
lift6 = lift . lift5
lift7 = lift . lift6
lift8 = lift . lift7
lift9 = lift . lift8


insert0 = lift
insert1 = MT.map insert0
insert2 = MT.map insert1
insert3 = MT.map insert2
insert4 = MT.map insert3
insert5 = MT.map insert4
insert6 = MT.map insert5
insert7 = MT.map insert6
insert8 = MT.map insert7
insert9 = MT.map insert8

foreverK f = let go a = f a >>= go in go