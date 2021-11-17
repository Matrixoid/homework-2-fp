module HW2.T4
  ( State(..)
  , Prim(..)
  , Expr(..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  )where

import HW2.T1 (Annotated(..), mapAnnotated)
import Control.Monad
import Control.Applicative

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState function S {runS = g} = S {runS = mapAnnotated function . g}

wrapState :: a -> State s a
wrapState a = S {runS = (a :#)}

joinState :: State s (State s a) -> State s a
joinState S {runS = gfun} = S {runS = \s -> helper (gfun s) `runS` phelp (gfun s)}
  where
    helper (a :# s) = a
    phelp (a :# s) = s

modifyState :: (s -> s) -> State s ()
modifyState fun = S {runS = \s -> () :# fun s}

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure    = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f  = joinState (fmap f m)
  return a = S {runS = (a :#)}

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y         = Op (Add x y)
  x - y         = Op (Sub x y)
  x * y         = Op (Mul x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y          = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x)   = pure x
eval (Op expr) = if isBinExpr expr then binOp expr else unarOp expr
  where
    isBinExpr :: Prim a -> Bool
    isBinExpr (Abs x) = False
    isBinExpr (Sgn x) = False
    isBinExpr _       = True
    binOp :: Prim Expr -> State [Prim Double] Double
    binOp (Add x y) = do
      leftValue  <- eval x
      rightValue <- eval y
      modifyState (Add leftValue rightValue :)
      return (leftValue + rightValue)
    binOp (Sub x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          modifyState (Sub leftValue rightValue :)
          return (leftValue - rightValue)
    binOp (Mul x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          modifyState (Mul leftValue rightValue :)
          return (leftValue * rightValue)
    binOp (Div x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          modifyState (Div leftValue rightValue :)
          return (leftValue / rightValue)
    unarOp :: Prim Expr -> State [Prim Double] Double
    unarOp (Abs x) = do
              valueX <- eval x
              modifyState (Abs valueX :)
              return (abs valueX)
    unarOp (Sgn x) = do
                  valueX <- eval x
                  modifyState (Sgn valueX :)
                  return (signum valueX)