{-# LANGUAGE GADTs, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | (this module's source should be read)
module Commands.Plugins.Example where
import Commands.Etc
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse
import Commands.Parse.Types
import Commands.Parsec

import Control.Monad.Catch (catch)

import Data.Typeable (Typeable)
import Control.Applicative ((<$>))


-- | demonstrates that grammars are first-class
positive :: Grammar Int
positive = NonTerminal (fromName 'positive) . fmap int . reverse $ [1..100]

data Command
 = Repeat Int Command
 | Edit Action Region
 | Click Times Button
 | ReplaceWith Phrase Phrase
 | Undo
 deriving (Show,Typeable)

-- | the root of our grammar (needs no annotation, and any grammar
-- can serve as root, when passed to the parser-generator
-- 'parsingWith').
-- 
-- note that it is directly self-referencing, safely terminating
-- thanks to laziness.
-- 
command :: Grammar Command
command = 'command
 #= Repeat      # positive  & command                    &e
 #| Edit        # action    & region                     &e
 #| Click       # times     & button  & "click"          &e
 #| ReplaceWith # "replace" & phrase  & "with"  & phrase &e
 #| Undo        # "undo"                                 &e

-- | we can derive an enumeration grammar with a single overloaded
-- value: 'terminals'. this saves a lot of boilerplate, while
-- preserving reuse and safety.
data Action = Copy | Delete | Next deriving (Show,Enum,Typeable)
action = terminals :: Grammar Action

data Region = Char | Word | Line deriving (Show,Enum,Typeable)
region = terminals :: Grammar Region

data Times = Single | Double | Triple deriving (Show,Enum,Typeable)
times = terminals :: Grammar Times

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Typeable)

button :: Grammar Button
button = 'button
 #= LeftButton # "left" &e
 #| MiddleButton # "middle" &e
 #| RightButton # "right" &e
-- (NonEmpty String)
newtype Dictation = Dictation [String] deriving (Show,Typeable)

-- | a "built-in" grammar with a context-sensitive parser
-- 'parseDictation'. though any user can define it in two lines of code.
dictation :: Grammar Dictation
dictation = sink

-- | it keeps parsing words until it could parse the context, at which
-- point it doesn't parse the context and stops parsing words.
-- 
-- e.g. in 'command', the first dictation has a context of "with".
parseDictation :: Some Parsec -> Parsec Dictation
parseDictation context = Dictation <$> anyWord `manyUntil` context

-- | here we say that we won't use the default automatically generated
-- parser for the dictation grammar, but instead our own specialized one.
overridingParsers = parseDictation `overrides` dictation

-- | parser-generator specialized to our grammar.
-- a simple convenience function.
parses = parsingWith overridingParsers (Just . Some $ eof) (Some . Parser $ eof)

type Phrase = Dictation
phrase = dictation

data Directions = Directions Phrase Phrase Phrase deriving (Show,Typeable)
directions :: Grammar Directions
directions = 'directions
 #= Directions # "from" & phrase & "to" & phrase & "by" & phrase & e
-- permute order of effects but not order of results! action-permutations
-- and how do we group the prepositions?


-- | (we test the grammar with an executable, as we can't test grammar
-- with doctest because of TemplateHaskell:
-- "You can't use Template Haskell with a profiled compiler").
-- 
main :: IO ()
main = do
 putStrLn ""
 (print =<< command `parses` "unknown") `catch` (\(e :: ParseError) -> print e)
 putStrLn ""
 (print =<< command `parses` "replace this no-with") `catch` (\(e :: ParseError) -> print e)
 putStrLn ""
 (print =<< command `parses` "replace with phrase-can't-have-zero-words") `catch` (\(e :: ParseError) -> print e)
 putStrLn ""
 print =<< command `parses` "delete word"
 print =<< command `parses` "10 next word"
 print =<< command `parses` "double left click"
 print =<< command `parses` "replace this and that with that and this"
 print =<< command `parses` "undo"
