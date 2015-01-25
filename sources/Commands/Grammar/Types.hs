{-# LANGUAGE GADTs, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Commands.Grammar.Types where
import Data.Vinyl.Filter

import Data.Vinyl


-- | A <http://en.wikipedia.org/wiki/Formal_grammar#Formal_definition formal grammar>.
-- Since we want to parse (and use the parsed result
-- of) a grammar, not just recognize the grammar, our grammars are also
-- labeled (cf. 'RHS').
-- 
-- Each @grammar@ term of type @Grammar@ __must have a unique 'LHS'__:
-- the 'Eq' @instance@ compares the 'LHS's of two 'NonTerminal's.
-- Consumers of 'Grammar's will assume the 'LHS's are disjoint.
-- 
-- 
--
-- Laziness lets the user write recursive grammars that directly reference
-- any other grammar (or themselves). Without non-termination, when
-- passed to a function. and without indirect references or mutation.
-- 
-- Mutually-recursive with 'RHS'.
-- 
data Grammar a where
 Terminal    :: !String -> Grammar String
 NonTerminal :: !LHS -> [RHS a] -> Grammar a

instance Eq (Grammar a) where
 Terminal    x   == Terminal    y   = x == y
 NonTerminal x _ == NonTerminal y _ = x == y
 _               == _               = False

-- | The left-hand side of the non-terminal should be a globally
-- unique identifier. We can implement this specification with Haskell
-- identifies. This lets the host language guarantee
-- uniqueness by using @Name@s, but only if we must provide them
-- correctly.
--
data LHS        = LHS !Identifier !Module !Package deriving (Show, Eq, Ord)
type Package    = String
type Module     = String
type Identifier = String

-- | A labeled right-hand side in a non-terminal.
--
-- See also <http://hackage.haskell.org/package/BNFC-meta LBNF>.
--
-- Uses @vinyl@ 'Rec'ords, rather than tuples, because:
-- 
-- * it's easy to write a single function on records inductively
-- (i.e. @Rec f \'[]@ and  @Rec f (x \': xs)@), rather than multiple
-- methods, one per each tuple of some length (i.e. @()@ and
-- @Identity@ and @(,)@ and @(,,)@ and @(,,,)@ and etc.). Relatedly, I
-- can define length-generic types, like @Rec Grammar xs@.
-- * (less importantly) the singleton @'HList' \'[x]@ is a distinct
-- type that fits in syntactically with the rest, unlike the singleton
-- tuple.
-- 
-- A @GADT@, to expose the 'RFilter' @Constraint@ upon pattern
-- matching. this @Constraint@, with the @label@ type, enforces the
-- meaning of the label as a constructor (or any function) that
-- "injects" the types of the sub-grammars into the type of our
-- grammar.
-- 
-- By existentially-quantifing the sub-grammars, we can put different
-- right-hand sides into a normal homogeneous list as in 'NonTerminal'.
-- each of which carries a constraint to filter away the 'Terminal's
-- (in the context of parsing), which are not injected into @a@.
-- 
-- I guess it's a mix between holding a constraint:
-- 
-- @data SomeShow where SomeShow :: forall x. Show x => x@
-- 
-- and holding an eliminator:
-- 
-- @data SomeString = forall x. SomeString x (x -> String)@
-- 
-- as it holds both.
-- 
-- an @RHS a@ takes:
-- 
-- * a @label@, for injecting into the type @a@
-- * some @grammars@, under a type-list @xs@
-- 
-- Mutually-recursive with 'Grammar'.
-- 
data RHS a where
 RHS :: forall a xs. (RFilter String xs)
     => (HList (Filter String xs) -> a)
     -> Rec Grammar xs
     -> RHS a