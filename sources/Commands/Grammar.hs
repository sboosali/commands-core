{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, ExistentialQuantification, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- 
-- The relative fixities of the syntactic sugar are:
-- 
-- (fixity of @&@) ≥ (fixity of @#@) ≥ (fixity of @#=@) ≥ (fixity of @#|@)
-- 
-- where "fixity is greater than" means "binds more tightly than"
-- (cf. @infixr 0 '$'@). So:
-- 
-- @
-- command = 'command
--  \#= Edit        \# action    & region                     &e
--  \#| Click       \# times     & button  & "click"          &e
-- @
-- 
-- parses as:
-- 
-- @
-- command  =  ('command \#= (Edit \# (action & (region &e))))  \#|  (Click \# (times & (button & ("click" &e))))
-- @
-- 
-- 
module Commands.Grammar where
import Commands.Grammar.Types
import Data.Vinyl.Prelude
import Data.Vinyl.Uncurry

import Data.Vinyl
import Data.Monoid ((<>))

import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Name(..),NameFlavour(NameG),OccName(..),PkgName(..),ModName(..))


infixr 9 &
infix  4 #
infix  3 #= 
infixl 2 #|


-- | sugar for right-appending to 'RHS's.
(#|) :: Grammar a -> RHS a -> Grammar a
Terminal s #| _ = Terminal s
NonTerminal l rs #| r = NonTerminal l (rs <> [r])

-- | sugar for 'NonTerminal'.
-- 
-- e.g.
-- 
-- @
-- verbatim = 'verbatim #= Dictation \# "say" & dictation & e
-- @
-- 
-- partial function via 'fromName'.
-- 
-- after working hard to make most functions type safe, why a
-- partial function? I want the 'LHS's to have two properties:
-- 
-- * disjoint (for correctness of serialization and of dynamically
-- dispatching specialized parsers)
-- * readable (for debugging/understanding the serialized grammars)
-- 
-- quoting global names uniquely with TemplateHaskell (e.g.
-- @global = 'global #= ... -- top-level declaration@) satisfies the
-- readability property. and it satisfies the disjointness property,
-- but only if the user:
-- 
-- * only quotes top-level-declared names (no @where local = 'local #= ...@)
-- * doesn't construct different 'Grammar's with the same 'Name' (e.g.
-- watching out for typos when the refactoring: typos on unbound names
-- are caught, but not collisions with other, bound, names)
-- 
-- that's the bad part. the good part is that it's easy, and that it's
-- "local":
-- 
-- * given that the user will be writing/editing dozens of grammars,
-- you want 'Grammar's to be easy-to-write and easy-to-read. typing an
-- extra @'name@ is.
-- * and given that the user might want to import grammars from other,
-- separately-compiled, modules, they can. they don't need to
-- uniquely generate the 'LHS's in some monad (safe but "global") or with
-- @unsafePerformIO@ procedures ("local" but unsafe) that must not be
-- inlined.
-- 
-- /alternatively.../
-- 
-- if users get mad at run-time errors or hard-to-debug serialization
-- corruption or something, I may write a safer TemplateHaskell
-- splicer anyway, or even a quasi-quoter for simple grammars.
-- I want to minimize the TemplateHaskell: so the pattern above uses
-- quoting only, not splicing.
-- 
-- it might even look like: @global = $lhs #= ...@ if TemplateHaskell
-- can be sufficiently hacked. or something that implicitly reifies
-- bindings.
-- 
(#=) :: Typeable a => Name -> RHS a -> Grammar a
name #= r = NonTerminal (fromName name) [r]

-- | sugar for 'RHS'.
(#) :: forall a xs. (RFilter String xs, Uncurry (Filter String xs))
     => (Curried (HList (Filter String xs) -> a))
     -> Rec Grammar xs
     -> RHS a
label # symbols = RHS (uncurryN label) symbols

-- | a partial function: only matches global names, i.e. @Name _
-- ('NameG' _ _ _)@.
-- 
-- should be injective, i.e. preserves uniqueness.
fromName :: Name -> LHS
fromName (Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod))) = LHS occ mod pkg

-- | sugar for ':&'.
-- 
-- implicitly injects types into 'Grammar' with 'toG'. thus:
-- 
-- @(grammar :: Grammar a) & "token" & e@
-- 
-- is well-typed, but:
-- 
-- @(nonGrammar :: Int) & "token" & e@
-- 
-- is not.
-- 
(&) :: (Grammatical a x) => a -> Rec Grammar xs -> Rec Grammar (x ': xs)
x & y = toGrammar x :& y

-- | class for implicitly injecting different types into 'Grammar's.
--
class    Grammatical a x | a -> x           where  toGrammar :: a -> Grammar x
instance Grammatical String          String where  toGrammar = Terminal
instance Grammatical (Grammar x) x          where  toGrammar = id

-- | you can read 'e' as either:
-- 
-- * the "'e'nd" of a grammar
-- * "'e'psilon" i.e. the "'e'mpty string"
-- 
-- the name is short because:
-- 
-- * you must type it a lot
-- * it's almost meaningless, and so it shouldn't distract your eye
-- from the meaningful 'Terminal's and 'NonTerminal's nearby.
-- 
-- @e = RNil@
-- 
e :: Rec Grammar '[]
e = RNil

-- | lifts a 'con'structor to a right-hand side.
con :: (Show a) => a -> RHS a
con c = c # show c & e

-- | specialized 'con' for type inference: Haskell integer literals
-- are polymorphic: @0 :: (Integral a) => a@ not @0 :: Integer@.
-- 
-- 
int :: Int -> RHS Int
int = con
