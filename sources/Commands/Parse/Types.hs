{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances                                         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Parse.Types where
import Commands.Etc
import Commands.Parsec     (Parsec, parserUnit, parserZero)
import Control.Applicative
import Data.Maybe          (fromMaybe)
import Data.Monoid         (First (..), Monoid, mappend, mempty, (<>))


-- | Parsers with restricted right-context-sensitivity.
--
-- 'RightContext'
-- existentially-quantifies its 'Parser', so the 'SensitiveParser' can
-- run the parser to check whether it succeeds or fails, but it can't
-- use any parse result. Neither should well-behaved
-- 'SensitiveParser's consume any tokens with the 'RightContext':
-- they can wrap the 'RightContext' in a call to
-- 'Text.Parsec.lookAhead'.
--
-- The type is isomorphic to @Compose (Reader Context) Parsec@. But
-- it's @Applicative@ instance is different (see
-- 'applySensitiveParser').
--
-- The trick is that in @Reader r f@, the environment @r@ and the
-- functor @f@ are independent. But here, they are dependent:
-- @r ~ Some f@.
--
-- Now we must prove that SensitiveParser is 'Applicative' and
-- 'Alternative'. see 'proof_Applicative_SensitiveParser'
-- and 'proof_Alternative_SensitiveParser'.
--
-- the Applicative instance can't be derived from Product.


-- |
type Context = Some Parsec

-- | goes from Alternative to Monoid. The inverse of Const:
-- which goes from Monoid to Alternative.
--
-- casts both existentially-quantified parsers' results to the same
-- concrete type (ignoring them) to combine them.
instance Monoid Context where
 mempty = Some parserZero
 mappend (Some x) (Some y) = Some ((parserUnit <* y) <|> (parserUnit <* x))

unless :: c -> First c -> c
unless x = fromMaybe x . getFirst

-- | (Nothing is identity in the Monoid instance for Maybe,
-- like logical-or)
alterFirstContext :: First Context -> First Context -> First Context
alterFirstContext (First x) (First y) = First $ x <> y


data SensitiveParser a = SensitiveParser (First Context) (Context -> Parsec a)
 deriving (Functor)

instance Applicative SensitiveParser where
 pure  = pureSensitiveParser
 (<*>) = applySensitiveParser

instance Alternative SensitiveParser where
 empty = emptySensitiveParser
 (<|>) = alterSensitiveParser

pureSensitiveParser :: a -> SensitiveParser a
pureSensitiveParser x = SensitiveParser mempty (\_ -> pure x)

applySensitiveParser :: SensitiveParser (a -> b) -> SensitiveParser a -> SensitiveParser b
applySensitiveParser (SensitiveParser fc fp) (SensitiveParser xc xp) = SensitiveParser
 (fc <> xc)
 (\zc -> fp (mempty `unless` xc) <*> xp zc)

emptySensitiveParser :: SensitiveParser a
emptySensitiveParser = SensitiveParser mempty (const empty)

alterSensitiveParser :: SensitiveParser a -> SensitiveParser a -> SensitiveParser a
alterSensitiveParser (SensitiveParser xc xp) (SensitiveParser yc yp) = SensitiveParser
 (alterFirstContext xc yc)
 (\c -> xp c <|> yp c)

