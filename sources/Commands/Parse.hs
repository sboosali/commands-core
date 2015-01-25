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
import Data.Maybe (fromMaybe)


-- | __g__rammar parser
-- 
--
gparser :: IsRoot -> Grammar a -> SensitiveParser a
gparser _      (Terminal s)       = SensitiveParser (\_ -> Parser $ word s)
gparser isRoot (NonTerminal _ rs) = SensitiveParser (\x -> nparser isRoot x rs)

-- | __n__on-terminal parser
-- 
-- calls 'try' on the 'Parser' induced by the right-hand-sides @[RHS
-- a]@
-- 
nparser :: forall a. IsRoot -> RightContext -> [RHS a] -> Parser a
nparser isRoot context rs = choice_ parsers
 where

 parsers :: [Parser a]
 parsers = fmap (lparser isRoot context) rs

 choice_ :: [Parser a] -> Parser a
 choice_ = Parser . try . choice . fmap parsec

-- | __l__abel parser
-- 
-- calls 'try' on the 'Parser' induced by the right-hand-side @RHS a@
-- 
-- 
lparser :: forall a. IsRoot -> RightContext -> RHS a -> Parser a
lparser isRoot context (RHS label grammars) = case fromMaybe (Some unitParser) isRoot of
 -- uses case, not a pattern, because of this error: "I can't handle pattern bindings for existential or GADT data constructors."
 Some q -> Parser (inject <$> try (p <* q))
 where
 Parser p = rhoist (rparser isRoot context grammars)
 inject = label . rfilter (Proxy :: Proxy String)

-- | __r__ecord parser
-- 
-- calls @'rmap' ('gparser' Nothing) grammars@. given the mutually
-- recursive call stack:
-- 
-- @
-- 'gparser'
-- 'nparser'
-- 'lparser'
-- 'rparser'
-- 'gparser'
-- ...
-- @
-- 
-- the value of the 'IsRoot' parameter, as called by 'parsing', is:
-- 
-- @
-- 'gparser' (Just (Some 'eof'))
-- 'gparser' Nothing
-- 'gparser' Nothing
-- ...
-- @
-- 
-- i.e. only non-@Nothing@ at first call, which is at the root of the
-- grammar in @'parsing' grammar@. thus, every 'RHS' of the root must
-- end exactly at the 'eof', which:
-- 
-- * rejects strict suffixes
-- * increases correctness by backtracking and by decreasing greediness
-- 
-- e.g.
-- 
-- >>> import Commands.Grammar
-- >>> let test = NonTerminal (LHS "" "" "") [int 1, int 10]
-- >>> test `parsing` "10" -- grammar didn't need {{reverse}}, {{int 1}} failed non-greedily
-- 10
-- >>> test `parsing` "100"
-- *** Exception: "100" (line 1, column 3):
-- unexpected '0'
-- expecting space or end of input
-- 
-- 
rparser :: forall xs. IsRoot -> RightContext -> Rec Grammar xs -> Rec Parser xs
rparser _ context grammars = freeParsers
 where

 freeParsers :: Rec Parser xs
 freeParsers = rscanr_ runSensitiveParser context sensitiveParsers

 -- specialized/annotated for clarity
 rscanr_ :: (forall x. SensitiveParser x -> RightContext -> Parser x) -> RightContext -> Rec SensitiveParser xs -> Rec Parser xs
 rscanr_ = rscanr

 sensitiveParsers :: Rec SensitiveParser xs
 sensitiveParsers = rmap (gparser Nothing) grammars

-- | the @grammar@ input is the root of that grammar which is being
-- parsed.
-- 
-- passes 'eof' as the 'IsRoot' to 'gparser', and 'eof' as the
-- 'RightContext' to a 'SensitiveParser'.
-- 
-- 
parsing :: Grammar a -> String -> Possibly a
parsing grammar s = parse fp s
 where
 Parser fp = sp (Some . Parser $ eof)
 SensitiveParser sp = gparser (Just . Some $ eof) grammar
