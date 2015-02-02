{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveDataTypeable #-}
module Commands.Parse.Types
 ( Parser(..)
 , mapParser

 , SensitiveParser(..)

 , RightContext
 , IsRoot

 , OverridingParsers()
 , overrides
 , overriddenBy
 ) where

import Commands.Parsec (Parsec)
import Commands.Etc
import Commands.Grammar.Types

import Control.Applicative (Applicative)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Dynamic
import Data.Monoid (Monoid)


-- | a context-sensitive 'Parser'.
--
-- 'RightContext'
-- existentially-quantifies its 'Parser', so the 'SensitiveParser' can
-- run the parser to check whether it succeeds or fails, but it can't
-- use any parse result.
--
-- Well-behaved 'SensitiveParser's should not
-- consume any tokens with the 'RightContext' either, by wrapping it
-- in a call to 'Text.Parsec.lookAhead'.
-- 
data SensitiveParser a = SensitiveParser { runSensitiveParser :: (RightContext -> Parser a) }
 deriving (Typeable)

-- | convenience constructor, hiding the @newtype@ boilerplate.
makeParser :: (Some Parsec -> Parsec a) -> SensitiveParser a
makeParser p = SensitiveParser (\(Some (Parser q)) -> Parser (p (Some q)))

-- | a @newtype@, not a @type@, because only type constructors, not
-- type aliases, can parameterize 'Data.Vinyl.Rec'ords.
-- 
newtype Parser a = Parser { parsec :: Parsec a } deriving (Functor, Applicative)

-- | lifts functions from 'Parsec' to 'Parser'. cf 'fmap', which
-- lifts functions from @a -> b@ to @Parser a -> Parser b@.
mapParser :: (Parsec a -> Parsec b) -> Parser a -> Parser b
mapParser f = Parser . f . parsec

-- |
type RightContext = Some Parser

-- | whether we are parsing at the root of a grammar.
-- 
-- a boolean with provenance (see
-- <http://www.stephendiehl.com/what/#boolean-blindness Boolean Blindness>)
-- 
-- 
type IsRoot = Maybe (Some Parsec)

-- | a set of 'SensitiveParser's, at most one per 'Grammar', that
-- override the default @Grammar@-generic
-- 'Commands.Parse.gparser'-generation.
-- 
-- 'OverridingParsers' is abstract, exporting:
-- 
-- * 'overrides' for introducing
-- * 'overriddenBy' for eliminating
-- * the Monoid instance for introducing/transforming.
-- 
newtype OverridingParsers = OverridingParsers (Map (Some Grammar) (Exists SensitiveParser))
 deriving (Monoid)

-- | introduces 'OverridingParsers'.
-- 
-- e.g.
-- 
-- @
-- 
-- overridden :: OverridingParsers
-- overridden
--  =  p1 \`overrides` g1
--  <> p2 \`overrides` g2
--  <> ...
-- 
-- @
-- 
overrides :: (Typeable x) => (Some Parsec -> Parsec x) -> Grammar x -> OverridingParsers
overrides p g = OverridingParsers $ Map.singleton (Some g) (Exists (makeParser p))

-- | eliminates 'OverridingParsers'.
-- 
-- 
overriddenBy :: (Typeable x) => Grammar x -> OverridingParsers -> Maybe (SensitiveParser x)
overriddenBy g (OverridingParsers ps) = Map.lookup (Some g) ps >>= (\(Exists p) -> cast p)

