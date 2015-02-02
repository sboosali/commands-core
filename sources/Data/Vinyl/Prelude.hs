{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs #-}
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

import Control.Applicative hiding (Const)
import GHC.Exts

-- $setup
-- for doctest:
-- 
-- >>> :set -XDataKinds -XExtendedDefaultRules
-- >>> import Data.Typeable (Proxy(..))
-- >>> import Data.Word (Word)

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

-- | like 'reifyConstraint', but the constraint is on the uninterpreted field
-- @r@, not the interpreted field @f r@. helps avoid unnecessary
-- 'Identity' instances when working with 'HList's.
reifyConstraintH
  :: All rs c
  => proxy c
  -> HList rs
  -> Rec (Dict c) rs
reifyConstraintH proxy record =
  case record of
    RNil -> RNil
    (Identity x :& xs) -> Dict x :& reifyConstraintH proxy xs

-- | like 'Dict', but the constraint is on the uninterpreted field
-- @a@, not the interpreted field @f a@. 
data Constrained c f a where
 Constrained :: c a => f a -> Constrained c f a

-- | 
reifyConstraintU
  :: All rs c
  => proxy c
  -> Rec f rs
  -> Rec (Constrained c f) rs
reifyConstraintU proxy record =
  case record of
    RNil -> RNil
    (x :& xs) -> Constrained x :& reifyConstraintU proxy xs


-- | inverts @'reifyConstraint (Proxy :: Proxy c)' forall @c@.
hideConstraint :: Rec (Dict c :. f) xs -> Rec f xs
hideConstraint = rmap (\(Compose (Dict x)) -> x)

-- | 'scanr' for records.
-- 
-- the accumulator must be existentially quantified via 'Some', because
-- 
--
-- 
-- e.g.
-- 
-- >>> recordToList . rscanr (\(Compose (Dict x)) (Some (Const y)) -> Const (show x ++ y)) (Some (Const "")) . reifyConstraint (Proxy :: Proxy Show) $ ('0' `cons` () `cons` True `cons` (3::Int) `cons` RNil)
-- ["'0'()True3","()True3","True3","3"]
-- 
-- equivalent to 'scanr' on constrained homogeneous records:
-- 
-- >>> let homogeneous   xs z = init . scanr (+) z . recordToList $ xs
-- >>> let heterogeneous rs z = recordToList . rscanr (\(Dict x) (Some (Const y)) -> Const (toInteger x + y)) (Some (Const z)) . reifyConstraintH (Proxy :: Proxy Integral) $ rs
-- >>> heterogeneous (Identity (4 :: Integer) :& Identity (3 :: Int) :& Identity (2 :: Word) :& RNil) 1
-- [10,6,3]
-- >>> homogeneous      (Const  4             :&    Const  3 :&            Const  2          :& RNil) 1
-- [10,6,3]
-- 
-- (if you squint, the @(Some (Const ...))@
-- and @*Constrain*@ and @recordToList@ noise fades;
-- that's just the sound of the "impedance mismatch" between
-- heterogeneous and homogenous collections).
-- 
rscanr :: forall f g xs. (forall x. f x -> Some g -> g x) -> Some g -> Rec f xs -> Rec g xs
rscanr f z xs = scan f xs
 where
 scan :: forall ys. (forall x. f x -> Some g -> g x) -> Rec f ys -> Rec g ys
 scan _ RNil      = RNil
 scan f (x :& xs) = case scan f xs of
  RNil      -> f x z             :& RNil
  (y :& ys) -> f x (Some y) :& y :& ys

