{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Mem where

class (Eq (Ref r), Monad r) => Mem r where
  data family Ref r :: *
  type family Val r :: *

  ref   :: Val r -> r (Ref r)
  deref :: Ref r -> r (Val r)
  set   :: Ref r -> Val r -> r ()

alter :: Mem r => (Val r -> Val r) -> Ref r -> r ()
alter f r = do
  v <- deref r
  set r (f v)
