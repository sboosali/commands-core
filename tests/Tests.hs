module Main where
import Commands.Test.Properties
import Test.DocTest
import Test.Tasty
import Test.Tasty.QuickCheck


tastytest = defaultMain $ testGroup "QuickCheck"
 [ testProperty "serialized DNSGrammar is valid Python" prop_DNSGrammar
 ]

main = do
 doctest
  [ "sources/Commands"
  , "sources/Commands/Etc"
  , "sources/Commands/Instances"

  , "sources/Commands/Grammar/Types"
  , "sources/Commands/Grammar"

  , "sources/Commands/Parse/Types"
  , "sources/Commands/Parse"
  , "sources/Commands/Parsec"

  , "sources/Commands/Frontends/Dragon13"
  , "sources/Commands/Frontends/Dragon13/Types"
  , "sources/Commands/Frontends/Dragon13/Lens"
  , "sources/Commands/Frontends/Dragon13/Text"
  , "sources/Commands/Frontends/Dragon13/Render"
  , "sources/Commands/Frontends/Dragon13/Optimize"
  , "sources/Commands/Frontends/Dragon13/Shim"

  , "sources/Commands/Command"
  , "sources/Commands/Command/Types"
  , "sources/Commands/Command/Sugar"
  , "sources/Commands/Command/Combinator"

  , "sources/Commands/Munging"

  , "sources/Commands/Plugins/Example"

  , "sources/Control/Alternative/Free/Tree"
  , "sources/Commands/Graph"
  ]

 -- tastytest TODO
