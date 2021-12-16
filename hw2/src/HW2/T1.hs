module HW2.T1
  ( Option(..)
  , Pair(..)
  , Quad(..)
  , Annotated(..)
  , Except(..)
  , Prioritised(..)
  , Stream(..)
  , List(..)
  , Fun(..)
  , Tree(..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  )where

data Option a = None | Some a
data Pair a = P a a
data Quad a = Q a a a a
data Annotated e a = a :# e
infix 0 :#
data Except e a = Error e | Success a
data Prioritised a = Low a | Medium a | High a
data Stream a = a :> (Stream a)
infixr 5 :>
data List a = Nil | a :. List a
infixr 5 :.
data Fun i a = F (i -> a)
data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None              = None
mapOption function (Some opt) = Some (function opt)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair function (P num1 num2) = P (function num1) (function num2)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad function (Q num1 num2 num3 num4) = Q (function num1)
                                             (function num2)
                                             (function num3)
                                             (function num4)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated function (a :# e) = function a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept function (Error err)   = Error err
mapExcept function (Success res) = Success (function res)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised function (Low low)    = Low (function low)
mapPrioritised function (Medium med) = Medium (function med)
mapPrioritised function (High high)  = High (function high) 

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream function (streamBegin :> streamTail) = function streamBegin 
                                               :> mapStream function streamTail

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList function (listHead :. listTail) = function listHead :. mapList function listTail

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun function1 (F function2) = F (function1 . function2)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree function (Branch left element r) = Branch (mapTree function left) 
                                                  (function element)
                                                  (mapTree function r)
