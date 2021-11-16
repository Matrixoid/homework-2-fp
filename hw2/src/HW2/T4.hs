module HW2.T4 where

import HW2.T1 (Annotated(..), mapAnnotated)
import Control.Monad
import Control.Applicative

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState function S {runS = g} = S {runS = mapAnnotated function . g}

wrapState :: a -> State s a
wrapState a = S {runS = (a :#)}

joinState :: State s (State s a) -> State s a
joinState S {runS = g} = S {runS = \s -> h (g s) `runS` p (g s)}
  where
    h (a :# s) = a
    p (a :# s) = s

modifyState :: (s -> s) -> State s ()
modifyState fun = S {runS = \s -> () :# fun s}

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)
  return a = S {runS = (a :#)}

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a deriving Show

data Expr = Val Double | Op (Prim Expr) deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)