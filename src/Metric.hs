{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Metric (module GHC.TypeLits
              , Unit,
              , Metre
              , Second,
              , (:*)
              , (:/)
              , (.*)
              , (.+)
              , (./)
              , emb
              , rep
              , (:^:)
              , (:*:)
              , (:/:)) where
import GHC.TypeLits

data ((a :: * -> *) :* (b :: * -> *)) c = Times  c 
data ((a :: * -> *) :/ (b :: * -> *)) c = Div    c 
data Unit a     = Unit   a
data Metre a    = Metre  a
data Second a   = Second a

{- Ugly hack for now -}
instance (Repd (f a), Num (Rep (f a))) => Num (f a) where
  (+) = undefined
  (*) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
  fromInteger = emb . fromInteger

class Repd a where
  type Rep a :: *
  rep :: a -> Rep a
  emb :: Rep a -> a

instance (Repd (f a), Show (Rep (f a))) => Show (f a) where
  show = show . rep

instance Repd ((a :* b) c) where
  type Rep ((a :* b) c) = c
  rep (Times x) = x
  emb = Times

instance Repd ((a :/ b) c) where
  type Rep ((a :/ b) c) = c
  rep (Div x) = x
  emb = Div

instance Repd (Unit a) where
  type Rep (Unit a) = a 
  rep (Unit x) = x
  emb = Unit

instance Repd (Metre a) where
  type Rep (Metre a) = a
  rep (Metre x) = x
  emb = Metre

instance Repd (Second a) where
  type Rep (Second a) = a
  rep (Second x) = x
  emb = Second

type family (a :*: b) :: * -> * where
  (a :/ b) :*: (c :/ d) = (a :*: c) :/: (b :*: d)
  (a :/ b) :*: c        = (a :*: c) :/: b 
  (a :* b) :*: c        = a :*: (b :*: c)
  a :*: (b :/ c)        = (a :*: b) :/: c
  Unit :*: a            = a
  a :*: Unit            = a
  a :*: b               = a :* b 

type family (a :/: b) :: * -> * where
  (a :/ b) :/: (c :/ d) = (a :*: d) :/: (b :*: c)
  (a :/ b) :/: c        = a :/: (b :*: c)
  a :/: (b :/ c)        = (a :*: c) :/: b
  a :/: (c :* d)        = SortOut ((Upper ((EliminatedU a c) :/: d)) :/
                          ((EliminatedL a c) :*: (Lower ((EliminatedU a c) :/: d))))
  a :/: c               = SortOut ((Upper ((EliminatedU a c))) :/ (EliminatedL a c))

type family (SortOut a) :: * -> * where
  SortOut (a :/ Unit) = a
  SortOut a           = a

type family (Upper a) :: * -> * where
  Upper (a :/ b) = a
  Upper a        = a

type family (Lower a) :: * -> * where
  Lower (a :/ b) = b
  Lower a        = Unit

type family (EliminatedU a b) :: * -> * where
  EliminatedU a a        = Unit
  EliminatedU (a :* b) a = b
  EliminatedU (a :* b) c = a :*: (EliminatedU b c)
  EliminatedU a b        = a

type family (EliminatedL a b) :: * -> * where
  EliminatedL a a        = Unit
  EliminatedL (a :* b) a = Unit
  EliminatedL (a :* b) c = EliminatedL b c
  EliminatedL a b = b

type family (a :^: (n :: Nat)) :: * -> * where
  a :^: 0 = Unit
  a :^: n = a :*: (a :^: (n - 1))

type SameRep f g a = (Repd (f a), Repd (g a), Rep (f a) ~ a, Rep (g a) ~ a)

(.*) :: (Num a, SameRep f g a, Rep ((f :*: g) a) ~ a, Repd ((f :*: g) a)) => f a -> g a -> (f :*: g) a
a .* b = emb (rep a * rep b)

(.+) :: (Num a, SameRep f g a, (f :/: g) a ~ Unit a) => f a -> g a -> f a
a .+ b = emb (rep a + rep b)

(./) :: (Fractional a, SameRep f g a, Rep ((f :/: g) a) ~ a, Repd ((f :/: g) a)) => f a -> g a -> (f :/: g) a
a ./ b = emb (rep a / rep b)

-- Examples
x :: Metre Float
x = emb 5

y :: Second Float
y = emb 3

z :: Unit Float
z = emb 2

-- Note that ex0 .+ ex1 gives a type error even though they are both represented as floats!
ex0 :: Unit Float
ex0 = (x .* y ./ (y .* x)) .+ z

ex1 :: (Metre :/: (Second :^: 2)) Float
ex1 = x ./ (y .* y)
