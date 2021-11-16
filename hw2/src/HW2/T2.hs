<<<<<<< HEAD
module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (aV :# aDef, bV :# bDef) = (aV, bV) :# (bDef <> aDef)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error aErr, b)        = Error aErr
distExcept (a, Error bErr)        = Error bErr
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a,    Low b)    = Low (a, b)
distPrioritised (Low a,    Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (High a,   Low b)    = High (a, b)
distPrioritised (Low a,    High b)   = High (a, b)
distPrioritised (High a,   Medium b) = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a,   High b)   = High (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> aT, b :> bT) = (a, b) :> distStream (aT, bT)

combineLists :: List a -> List a -> List a
combineLists (a :. aT) b = a :. (combineLists aT b)
combineLists Nil b       = b

combineElement :: a -> List b -> List (a, b)
combineElement a (b :. bT) = (a, b) :. combineElement a bT
combineElement a Nil       = Nil

distList :: (List a, List b) -> List (a, b)
distList (Nil, b)       = Nil
distList (a :. aT, Nil) = Nil
distList (a :. aT, b)   = combineLists (combineElement a b) (distList (aT, b))

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F aFun, F bFun) = F (\x -> (aFun x, bFun x))

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated val = val :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream s = s :> wrapStream s

wrapList :: a -> List a
wrapList h = h :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F (\x -> a)
=======
module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (aV :# aDef, bV :# bDef) = (aV, bV) :# (bDef <> aDef)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error aErr, b)        = Error aErr
distExcept (a, Error bErr)        = Error bErr
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a,    Low b)    = Low (a, b)
distPrioritised (Low a,    Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (High a,   Low b)    = High (a, b)
distPrioritised (Low a,    High b)   = High (a, b)
distPrioritised (High a,   Medium b) = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a,   High b)   = High (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> aT, b :> bT) = (a, b) :> distStream (aT, bT)

combineLists :: List a -> List a -> List a
combineLists (a :. aT) b = a :. (combineLists aT b)
combineLists Nil b       = b

combineElement :: a -> List b -> List (a, b)
combineElement a (b :. bT) = (a, b) :. combineElement a bT
combineElement a Nil       = Nil

distList :: (List a, List b) -> List (a, b)
distList (Nil, b)       = Nil
distList (a :. aT, Nil) = Nil
distList (a :. aT, b)   = combineLists (combineElement a b) (distList (aT, b))

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F aFun, F bFun) = F (\x -> (aFun x, bFun x))

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated val = val :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream s = s :> wrapStream s

wrapList :: a -> List a
wrapList h = h :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F (\x -> a)
>>>>>>> a48a6408b5947c31b98bae09c179de7e82b0df44
