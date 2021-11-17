module HW2.T5
 ( ExceptState(..)
 , EvaluationError(..)
 , mapExceptState
 , wrapExceptState
 , joinExceptState
 , modifyExceptState
 , throwExceptState
 , eval
 ) where

import HW2.T1 (Except(..), Annotated(..), mapExcept, mapAnnotated)
import HW2.T4 (Expr(..), Prim(..))
import Control.Monad
import Control.Applicative

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState fun ES {runES = g} = ES {runES = mapExcept (mapAnnotated fun) . g}

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES {runES = \s -> Success (a :# s)}

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState ES {runES = g} = ES {runES = \s -> h (g s) `runES` p (g s)}
  where
    h (Error e) = ES {runES = \_ -> Error e}
    h (Success (a :# s)) = a
    p (Success (a :# s)) = s

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState fun = ES {runES = \s -> Success (() :# fun s)}

throwExceptState :: e -> ExceptState e s a
throwExceptState ex = ES {runES = \_ -> Error ex}

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)
  return a = ES {runES = \s -> Success (a :# s)}

data EvaluationError = DivideByZero deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x
eval (Op expr) = if isBinExpr expr then binOp expr else unarOp expr
  where
    isBinExpr :: Prim a -> Bool
    isBinExpr (Abs x) = False
    isBinExpr (Sgn x) = False
    isBinExpr _       = True
    binOp :: Prim Expr -> ExceptState EvaluationError [Prim Double] Double
    binOp (Add x y) = do
      leftValue  <- eval x
      rightValue <- eval y
      modifyExceptState (Add leftValue rightValue :)
      return (leftValue + rightValue)
    binOp (Sub x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          modifyExceptState (Sub leftValue rightValue :)
          return (leftValue - rightValue)
    binOp (Mul x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          modifyExceptState (Mul leftValue rightValue :)
          return (leftValue * rightValue)
    binOp (Div x y) = do
          leftValue  <- eval x
          rightValue <- eval y
          if rightValue == 0
          then throwExceptState DivideByZero
          else modifyExceptState (Div leftValue rightValue :)
          return (leftValue / rightValue)
    unarOp :: Prim Expr -> ExceptState EvaluationError [Prim Double] Double
    unarOp (Abs x) = do
              valueX <- eval x
              modifyExceptState (Abs valueX :)
              return (abs valueX)
    unarOp (Sgn x) = do
                  valueX <- eval x
                  modifyExceptState (Sgn valueX :)
                  return (signum valueX)