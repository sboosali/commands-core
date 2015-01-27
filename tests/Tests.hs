module Main where
import Test.DocTest


main = doctest
 [ "sources/Commands/Etc"
 , "sources/Commands/Instances"
 , "sources/Data/Vinyl/Filter"
 , "sources/Data/Vinyl/Prelude"
 , "sources/Data/Vinyl/Uncurry"
 , "sources/Commands/Grammar/Types"
 , "sources/Commands/Grammar"
 , "sources/Commands/Parse/Types"
 , "sources/Commands/Parse"
 , "sources/Commands/Parsec"
 , "sources/Commands/Munging"
 ]
