{-# LANGUAGE GADTs, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns, TupleSections #-}
module Commands.Plugins.Example where
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse
import Commands.Parsec (ParseError)

import Control.Monad.Catch (catch)
import Data.Vinyl
import Data.Vinyl.Functor

import Data.Typeable (Typeable)


-- | demonstrates that grammars are first-class
positive :: Grammar Int
positive = NonTerminal (fromName 'positive) . fmap int . reverse $ [1..100]

data Command
 = Repeat Int Command
 | Edit Action Region
 | Click Times Button
 | ReplaceWith Phrase Phrase
 | Undo ()
 deriving (Show,Typeable)

command :: Grammar Command
command = 'command
 #= (\(Identity n :& Identity c :& RNil) -> Repeat n c) # positive & command & e
 #| (\(Identity e :& Identity r :& RNil) -> Edit e r) # action & region & e
 #| (\(Identity t :& Identity b :& RNil) -> Click t b) # times & button & "click" & e
 #| (\(Identity this :& Identity that :& RNil) -> ReplaceWith this that) # "replace" & phrase & "with" & phrase & e
 #| (\RNil -> Undo ()) # "undo" & e

data Action = Copy | Delete | Next deriving (Show,Typeable)

action :: Grammar Action
action = 'action
 #= con Copy
 #| con Delete
 #| con Next

data Region = Char | Word deriving (Show,Typeable)

region :: Grammar Region
region = 'region
 #= con Char
 #| con Word

data Times = Single | Double | Triple deriving (Show,Typeable)

times :: Grammar Times
times = 'times
 #= con Single
 #| con Double
 #| con Triple

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Typeable)

button :: Grammar Button
button = 'button
 #= con LeftButton
 #| con MiddleButton
 #| con RightButton

newtype Phrase = Phrase String deriving (Show,Typeable)

phrase :: Grammar Phrase
phrase = 'phrase
 #= const (Phrase "this") # "this" & e
 #| const (Phrase "that") # "that" & e


main :: IO ()
main = do
 putStrLn ""
 (print =<< command `parsing` "unknown") `catch` (\(e :: ParseError) -> print e)
 putStrLn ""
 print =<< command `parsing` "Delete Word"
 print =<< command `parsing` "10 Next Word"
 print =<< command `parsing` "Double LeftButton click"
 print =<< command `parsing` "replace this with that"
 print =<< command `parsing` "undo"
 -- print =<< command `parsing` "replace this and that with that and this"
