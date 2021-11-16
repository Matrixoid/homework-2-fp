module HW2.T5 where

import HW2.T1 (Except(..), Annotated(..), mapExcept, mapAnnotated)
import Control.Monad

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

data EvaluationError = DivideByZero