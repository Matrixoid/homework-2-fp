{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6 where

import HW2.T5 (ExceptState(..))
import Numeric.Natural
import Data.Functor
import Control.Applicative
import Control.Monad

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a) deriving newtype (Functor, Applicative, Monad)
