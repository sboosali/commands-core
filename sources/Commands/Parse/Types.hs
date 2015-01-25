{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Commands.Parse.Types where
import Commands.Parsec (Parsec)
import Commands.Etc

import Control.Applicative (Applicative)


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
