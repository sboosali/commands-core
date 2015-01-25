{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.Vinyl.Uncurry where

import Data.Vinyl
import Data.Vinyl.Functor


-- | takes any function (implicitly curried by Haskell), and gives
-- back the function, uncurried (wrt a heterogenous list).
-- 
-- for example:
-- 
-- >>> (,) 'a' True
-- ('a',True)
-- 
-- >>> uncurryN (,) (Identity 'a' :& Identity True :& RNil)
-- ('a',True)
-- 
-- even works on values (nullary functions):
-- 
-- >>> uncurryN True RNil
-- True
-- 
-- records, unlike any-sized tuples, are "inductive". thus, the @N@ in
-- 'uncurryN' really means "any natural" (not "up to nine or so"),
-- as the method is defined inductively:
-- 
-- >>> :set -XTupleSections
-- >>> uncurryN (,,,,,,,,,) (Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& Identity () :& RNil)
-- ((),(),(),(),(),(),(),(),(),())
-- 
-- 
-- type inference for @Uncurry u@ needs the return type @(HList u ->
-- z)@ to be concrete (I think), as the type family 'Curried' is not
-- injective (nor are any type families, as of GHC 7.8).
-- i.e. GHC can't infer what type @(HList u -> z)@ must be from its
-- image @Curried (HList u -> z)@.
-- 
-- it violates laziness...
--
-- >>> const () undefined
-- ()
-- >>> uncurryN const (Identity () :& Identity undefined :& RNil)
-- *** Exception: Prelude.undefined
-- 
-- ...because 'Rec'ords are strict in their fields and/or 'Identity'
-- is a @newtype@. and @type 'HList' = 'Rec' 'Identity'@. (the error
-- comes from just the call @Identity undefined@ by the way).
-- I won't try to redefine @type HList = Rec Thunk@ or something, as I
-- don't need to preserve laziness for my use of uncurryN, and
-- strictness "at the leaves" should improve performance anyway.
-- 
-- I don't provide the other half of the isomorphism (at least, it
-- would be if it were lazy), i.e. @curryN@, because I don't
-- need it, and I don't know if the type inference works for currying
-- values like it does for uncurrying them.
-- 
-- 
class Uncurry u where
 uncurryN :: Curried (HList u -> z) -> (HList u -> z)

instance Uncurry '[]                     where  uncurryN function RNil      = function
instance Uncurry as => Uncurry (a ': as) where  uncurryN function (x :& xs) = uncurryN (function (getIdentity x)) xs

-- | defined for all @HList xs@.
-- 
-- >>> :kind! Curried (HList [Integer,Integer] -> Integer)
-- Curried (HList [Integer,Integer] -> Integer) :: *
-- = Integer -> Integer -> Integer
-- 
-- 
type family Curried hu where
 Curried (HList '[]       -> z) = z
 Curried (HList (x ': xs) -> z) = (x -> Curried (HList xs -> z))
