{-# LANGUAGE FlexibleInstances #-}

-- Determine if a valid Functor can be written.
-- 1 No
-- 2 Yes
-- 3 Yes
-- 4 Yes
-- 5 No

-- Rearrange the arguments to the type constructor of the
-- datatype so the Functor instance works.

-- 1

data Sum a b =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

-- 2

data Company a b c =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something a) = Something a

-- 3

data More a b =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

-- Write Functor instances
-- 1

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor a) = Bloor (f a)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
    
-- 2

data K a b =
  K a
  deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K a) = K a
  
-- 3

newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Show, Eq)

instance (Functor g, Functor f) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Show, Eq)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x $ fmap f y

-- 8

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Show, Eq)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

-- 9

data List a =
  Nil
  | Cons a (List a)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) =
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read (f . g)
