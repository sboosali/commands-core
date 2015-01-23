module Main where
import Test.DocTest


main = do
 doctest ["sources/Data/Vinyl/Filter", "sources/Data/Vinyl/Prelude"]
