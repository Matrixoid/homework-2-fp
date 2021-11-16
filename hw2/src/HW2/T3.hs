module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))
import HW2.T2 (distFun)

joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some o) = o

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error err)             = Error err
joinExcept (Success (Error err))   = Error err
joinExcept (Success (Success suc)) = Success suc

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((val :# def1) :# def2) = val :# (def1 <> def2)

combineLists :: List a -> List a -> List a
combineLists (h :. t) b = h :. (combineLists t b)
combineLists Nil b      = b

joinList :: List (List a) -> List a
joinList (Nil :. t) = joinList t
joinList (h   :. t) = combineLists h (joinList t)
joinList Nil        = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let (F inF) = f x
                             res = inF x
                         in res)
