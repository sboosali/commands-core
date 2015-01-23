{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | Safely uses @UndecidableInstances@ and @OverlappingInstances@.
--
-- TODO prove it
module Data.Vinyl.Filter (Filter, RFilter, rfilter) where

import Data.Vinyl


-- | type-level 'filter'.
type family Filter (t :: k) (xs :: [k]) :: [k] where
 Filter t '[]       =              '[]
 Filter t (t ': xs) =      Filter t xs
 Filter t (x ': xs) = x ': Filter t xs

-- | a filter on records by type.
-- 
-- a "closed" class, because I don't export its method.
-- 
-- there are three instances, which match the three cases of 'Filter'.
-- 
-- I don't know why we need the equality constraint, on the "skip @t@" instance.
-- There should be disequality between the "rigid type variables" @t@ and @x@, right?
-- 
class RFilter t xs where
 _rfilter :: proxy t -> HList xs -> HList (Filter t xs)

instance RFilter t '[] where _rfilter _ RNil = RNil

instance
 ( RFilter t xs
 ) =>
 RFilter t (t ': xs)
 where
 _rfilter proxy (_ :& xs) = _rfilter proxy xs

instance
 ( RFilter t xs
 , Filter t (x ': xs) ~ (x ': Filter t xs)
 ) =>
 RFilter t (x ': xs)
 where
 _rfilter proxy (x :& xs) = x :& _rfilter proxy xs

-- | 'rfilter' is a method. it filters 'HList's by type, not value.
-- 
-- >>> import Data.Typeable (Proxy(..))
-- >>> rfilter (Proxy :: Proxy ()) (Identity () :& Identity (1::Int) :& Identity () :& Identity True :& Identity () :& RNil)
-- {1; True}
-- 
rfilter :: (RFilter t xs) => proxy t -> HList xs -> HList (Filter t xs)
rfilter = _rfilter
