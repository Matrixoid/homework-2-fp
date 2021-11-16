<<<<<<< HEAD
module HW2.T1
  ( Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a = Nil | a :. List a
infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption fun None     = None
mapOption fun (Some o) = Some (fun o)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair fun (P a b) = P (fun a) (fun b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad fun (Q a b c d) = Q (fun a) (fun b) (fun c) (fun d)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated fun (val :# def) = (fun val) :# def

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept fun (Success suc) = Success (fun suc)
mapExcept fun (Error err)   = Error err

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised fun (Low p)    = Low (fun p)
mapPrioritised fun (Medium p) = Medium (fun p)
mapPrioritised fun (High p)   = High (fun p)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream fun (s :> sT) = fun s :> mapStream fun sT

mapList :: (a -> b) -> (List a -> List b)
mapList fun (h :. t) =  fun h :. mapList fun t
mapList fun Nil      = Nil

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun fun (F f) = F (\x -> fun (f x))

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree fun Leaf           = Leaf
mapTree fun (Branch l v r) = Branch (mapTree fun l) (fun v) (mapTree fun r)
=======
module HW2.T1
  ( Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a = Nil | a :. List a
infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption fun None     = None
mapOption fun (Some o) = Some (fun o)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair fun (P a b) = P (fun a) (fun b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad fun (Q a b c d) = Q (fun a) (fun b) (fun c) (fun d)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated fun (val :# def) = (fun val) :# def

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept fun (Success suc) = Success (fun suc)
mapExcept fun (Error err)   = Error err

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised fun (Low p)    = Low (fun p)
mapPrioritised fun (Medium p) = Medium (fun p)
mapPrioritised fun (High p)   = High (fun p)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream fun (s :> sT) = fun s :> mapStream fun sT

mapList :: (a -> b) -> (List a -> List b)
mapList fun (h :. t) =  fun h :. mapList fun t
mapList fun Nil      = Nil

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun fun (F f) = F (\x -> fun (f x))

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree fun Leaf           = Leaf
mapTree fun (Branch l v r) = Branch (mapTree fun l) (fun v) (mapTree fun r)
>>>>>>> a48a6408b5947c31b98bae09c179de7e82b0df44
