{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}
module Data.Vinyl.Prelude
 ( module Data.Vinyl.Prelude
 -- |
 , module Data.Vinyl.Filter
 ) where

import Commands.Etc
import Data.Vinyl.Filter

import Data.Vinyl
import Data.Vinyl.Functor

import Control.Applicative
import GHC.Exts


-- $setup
-- for doctest:
-- 
-- >>> :set -XDataKinds -XExtendedDefaultRules
-- >>> import Data.Typeable (Proxy(..))
-- >>> import Data.Word (Word)

-- | a heterogeneous @(:)@
cons :: x -> HList xs -> HList (x ': xs)
cons = (:&) . Identity
infixr 9 `cons`

-- | @rhoist = 'rtraverse' (fmap Identity)@
-- 
-- @fmap Identity@ specializes the transformation argument of
-- rtraverse to @:: f x -> f (Identity x)@
-- 
rhoist :: (Applicative f) => Rec f xs -> f (HList xs)
rhoist = rtraverse (fmap Identity)

-- | type-level @all@, which returns not a type of kind @Bool@, but a
-- "proof" of kind 'Constraint'.
-- 
-- like 'RecAll', but the constraint is on the uninterpreted field
-- @r@, not the interpreted field @f r@.
-- 
type family All (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  All '[]       c = ()
  All (r ': rs) c = (c r, All rs c)

-- | 'Prelude.scanr' for records
-- 
-- preserves the length of the 'Rec'ord.
-- 
-- >>> recordToList . rscanr (\(Compose (Dict x)) (Some (Const y)) -> Const (show x ++ y)) (Some (Const "")) . reifyConstraint (Proxy :: Proxy Show) $ ('0' `cons` () `cons` True `cons` (3::Int) `cons` RNil)
-- ["'0'()True3","()True3","True3","3"]
-- 
rscanr :: forall f g xs. (forall x. f x -> Some g -> g x) -> Some g -> Rec f xs -> Rec g xs
rscanr f z xs = scan f xs
 where
 scan :: forall ys. (forall x. f x -> Some g -> g x) -> Rec f ys -> Rec g ys
 scan _ RNil      = RNil
 scan f (x :& xs) = case scan f xs of
  RNil      -> f x z             :& RNil
  (y :& ys) -> f x (Some y) :& y :& ys

