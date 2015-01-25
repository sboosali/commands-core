{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Commands.Parse where
import Commands.Parse.Types
import Commands.Etc
import Commands.Grammar.Types
import Commands.Parsec
import Data.Vinyl.Prelude

import Data.Vinyl

import Data.Typeable (Proxy(..))
import Control.Applicative


-- | __g__rammar parser
-- 
--
gparser :: Grammar a -> SensitiveParser a
gparser (Terminal s)       = SensitiveParser (\_ -> Parser $ word s)
gparser (NonTerminal _ rs) = SensitiveParser (\x -> nparser x rs)

-- | __n__on-terminal parser
--
-- 
nparser :: forall a. RightContext -> [RHS a] -> Parser a
nparser context rs = choice_ parsers
 where

 parsers :: [Parser a]
 parsers = fmap (lparser context) rs

 choice_ :: [Parser a] -> Parser a
 choice_ = Parser . try . choice . fmap parsec

-- | __l__abel parser
-- 
-- 
lparser :: forall a. RightContext -> RHS a -> Parser a
lparser context (RHS label grammars) = Parser (inject <$> try p)
 where
 Parser p = rhoist (rparser context grammars)
 inject = label . rfilter (Proxy :: Proxy String)
 -- uses case, not a pattern, because of this error:
 --  My brain just exploded
 --  I can't handle pattern bindings for existential or GADT data constructors.
 --  Instead, use a case-expression, or do-notation, to unpack the constructor.
 -- uses case at outermost expression, because who cares about efficiency.

-- | __r__ecord parser
-- 
-- e.g.
-- 
-- >>> import Commands.Grammar
-- >>> let test = NonTerminal (LHS "" "" "") . map con . reverse $ [1..10] :: Grammar Integer
-- >>> test `parsing` "10"
-- 10
-- 
-- 
rparser :: forall xs. RightContext -> Rec Grammar xs -> Rec Parser xs
rparser context grammars = freeParsers
 where

 freeParsers :: Rec Parser xs
 freeParsers = rscanr_ runSensitiveParser context sensitiveParsers

 -- specialized/annotated for clarity
 rscanr_ :: (forall x. SensitiveParser x -> RightContext -> Parser x) -> RightContext -> Rec SensitiveParser xs -> Rec Parser xs
 rscanr_ = rscanr

 sensitiveParsers :: Rec SensitiveParser xs
 sensitiveParsers = rmap gparser grammars

-- |
-- 
-- the @grammar@ input is the root of that grammar which is being
-- parsed.
-- 
parsing :: Grammar a -> String -> Possibly a
parsing grammar s = parse fp s
 where
 Parser fp = sp (Some . Parser $ eof)
 SensitiveParser sp = gparser grammar

