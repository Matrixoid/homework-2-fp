module HW2.T3 
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW2.T1 ( Option(..)
              , Pair(..)
              , Quad(..)
              , Annotated(..)
              , Except(..)
              , Prioritised(..)
              , Stream(..)
              , List(..)
              , Fun(..))
import Data.Semigroup (Semigroup, (<>))

joinOption :: Option (Option a) -> Option a
joinOption None            = None
joinOption (Some None)     = None
joinOption (Some (Some a)) = Some a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)             = Error e
joinExcept (Success (Error e))   = Error e
joinExcept (Success (Success a)) = Success a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil        = Nil
joinList (a :. Nil) = a
joinList (a :. b)   = listConcat a (joinList b)
 where
   listConcat :: List a -> List a -> List a
   listConcat Nil l2         = l2
   listConcat (a1 :. l1t) l2 = a1 :. listConcat l1t l2

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let (F g) = f x
                             res = g x
                         in res)