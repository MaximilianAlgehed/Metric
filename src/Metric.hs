{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

data (a :* b) = Times (Rep a)
data (a :/ b) = Div   (Rep a)
data Unit a   = Unit  (Rep a)
data Metre a  = Metre (Rep a)
data Second a = Second (Rep a)

class Repd a where
  type Rep a :: *
  rep :: a -> Rep a
  emb :: Rep a -> a

instance Repd Int where
  type Rep Int = Int
  rep = id
  emb = id

instance Repd Float where
  type Rep Float = Float
  rep = id
  emb = id

instance (Repd a, Repd b, Rep a ~ Rep b) => Repd (a :* b) where
  type Rep (a :* b) = Rep a
  rep (Times x) = x
  emb = Times

instance (Repd a, Repd b, Rep a ~ Rep b) => Repd (a :/ b) where
  type Rep (a :/ b) = Rep a
  rep (Div x) = x
  emb = Div

instance (Repd a) => Repd (Unit a) where
  type Rep (Unit a) = Rep a
  rep (Unit x) = x
  emb = Unit

instance (Repd a) => Repd (Metre a) where
  type Rep (Metre a) = Rep a
  rep (Metre x) = x
  emb = Metre

instance (Repd a) => Repd (Second a) where
  type Rep (Second a) = Rep a
  rep (Second x) = x
  emb = Second

type family (a :*: b) :: * where
  (a :/ b) :*: (c :/ d) = (a :*: c) :/: (b :*: d)
  (a :/ b) :*: c        = (a :*: c) :/: b 
  (a :* b) :*: c        = a :*: (b :*: c)
  a :*: (b :/ c)        = (a :*: b) :/: c
  Unit x :*: a          = a
  a :*: Unit x          = a
  a :*: b               = a :* b 

type family (a :/: b) :: * where
  a :/: a               = Unit (Rep a)
  a :/: (Unit b)        = a
  (a :/ b) :/: (c :/ d) = (a :*: d) :/: (b :*: c)
  (a :/ b) :/: c        = a :/: (b :*: c)
  (a :* b) :/: (a :* c) = b :/: c
  (a :* b) :/: (c :* d) = SortOut ((Upper ((a :*: (EliminatedU b c)) :/: d)) :/ ((EliminatedL b c) :*: (Lower ((a :*: (EliminatedU b c)) :/: d))))
  (a :* b) :/: c        = SortOut ((Upper ((a :*: (EliminatedU b c)))) :/ (EliminatedL b c))
  x :/: y = x :/ y

type family (SortOut a) :: * where
  SortOut (a :/ (Unit x)) = a
  SortOut a               = a

type family (Upper a) :: * where
  Upper (a :/ b) = a
  Upper a        = a

type family (Lower a) :: * where
  Lower (a :/ b) = b
  Lower a        = Unit (Rep a)

type family (EliminatedU a b) :: * where
  EliminatedU a a        = Unit (Rep a)
  EliminatedU (a :* b) a = b
  EliminatedU (a :* b) c = a :*: (EliminatedU b c)
  EliminatedU a b        = a

type family (EliminatedL a b) :: * where
  EliminatedL a a        = Unit (Rep a)
  EliminatedL (a :* b) a = Unit (Rep a)
  EliminatedL (a :* b) c = EliminatedL b c
  EliminatedL a b = b

(.*) :: (Repd a, Repd b, Repd (a :*: b), Rep a ~ Rep b, Rep (a :*: b) ~ Rep a, Num (Rep a)) => a -> b -> (a :*: b)
a .* b = emb (rep a * rep b)

(.+) :: (Repd a, Num (Rep a)) => a -> a -> a
a .+ b = emb (rep a + rep b)

(./) :: (Repd a, Repd b, Repd (a :/: b), Rep a ~ Rep b, Rep (a :/: b) ~ Rep a, Fractional (Rep a)) => a -> b -> (a :/: b)
a ./ b = emb (rep a / rep b)

-- Example

x :: Metre Float
x = emb 5

y :: Second Float
y = emb 3

z :: Unit Float
z = emb 2
