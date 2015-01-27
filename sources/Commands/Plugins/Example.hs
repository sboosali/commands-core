{-# LANGUAGE GADTs, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Example where
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse
import Commands.Parsec (ParseError)

import Control.Monad.Catch (catch)

import Data.Typeable (Typeable)


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

command :: Grammar Command
command = 'command
 #= Repeat      # positive  & command                    &e
 #| Edit        # action    & region                     &e
 #| Click       # times     & button  & "click"          &e
 #| ReplaceWith # "replace" & phrase  & "with"  & phrase &e
 #| Undo        # "undo"                                 &e

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

newtype Phrase = Phrase String deriving (Show,Typeable)

phrase :: Grammar Phrase
phrase = 'phrase
 #= Phrase "this" # "this" &e
 #| Phrase "that" # "that" &e


-- | (we test the grammar with an executable, as we can't test grammar
-- with doctest because of TemplateHaskell:
-- "You can't use Template Haskell with a profiled compiler").
-- 
main :: IO ()
main = do
 putStrLn ""
 (print =<< command `parsing` "unknown") `catch` (\(e :: ParseError) -> print e)
 putStrLn ""
 print =<< command `parsing` "delete word"
 print =<< command `parsing` "10 next word"
 print =<< command `parsing` "double left click"
 print =<< command `parsing` "replace this with that"
 print =<< command `parsing` "undo"
 -- putStrLn ""
 -- print =<< command `parsing` "replace this and that with that and this"
