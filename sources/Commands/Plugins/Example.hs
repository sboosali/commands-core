{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Example where
import Commands.Etc
import Commands.Parse
import Commands.Parse.Types
import Commands.Parsec
import Control.Applicative
import Control.Applicative.Permutation
import Control.Exception               (assert)
import Control.Exception.Lens          (catching, handler, handling)
import Control.Monad.Catch             (catch, catches)
import Data.Foldable                   (asum)
import Data.List                       (intercalate)
import Data.Monoid                     ((<>))
import Data.Traversable                (traverse)


data Command
 = Undo
 | ReplaceWith Dictation Dictation
 | Repeat Positive Command
 deriving (Show,Eq)
command :: SensitiveParser Command
command
   = Undo         <$  terminal "undo"
 <|> ReplaceWith  <$> (terminal "replace" *> dictation) <*> (terminal "with" *> dictation)
 -- <|> Repeat       <$> positive <*> command

data Test = Test Dictation Command deriving (Show,Eq)
test = Test <$> dictation <*> command

newtype Positive = Positive Int deriving (Show,Eq)
positive :: SensitiveParser Positive
positive = Positive <$> (asum . map int) [1..9]

newtype Dictation = Dictation [String] deriving (Show,Eq)
dictation :: SensitiveParser Dictation
dictation = SensitiveParser
 (contextual anyWord)
 (\context -> Dictation <$> anyWord `manyUntil` context)



-- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
data DirectionsS = DirectionsS Dictation Dictation Dictation deriving (Show,Eq)
directionsS :: SensitiveParser DirectionsS
directionsS = terminal "directions" *> (runPerms $ DirectionsS
 <$> atom (terminal "from" *> dictation)
 <*> atom (terminal "to"   *> dictation)
 <*> atom (terminal "by"   *> dictation))

data Place = Place String deriving (Show,Eq)
place :: SensitiveParser Place
place = Place <$> freely anyWord

data Transport = Foot | Bike | Bus | Car deriving (Show,Eq,Enum)
transport :: SensitiveParser Transport
transport = twig

-- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtom'
data DirectionsF = DirectionsF (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
directionsF :: SensitiveParser DirectionsF
directionsF = terminal "directions" *> (runPerms $ DirectionsF
 <$> maybeAtom (terminal "from" *> place)
 <*> maybeAtom (terminal "to"   *> place)
 <*> maybeAtom (terminal "by"   *> transport))


exampleDirections =
 [ "directions from here to there  by bike"
 , "directions from here by bike   to there"
 , "directions to there  from here by bike"
 , "directions to there  by bike   from here"
 , "directions by bike   from here to there"
 , "directions by bike   to there  from here"
 ]

goodDirections  = DirectionsS  (Dictation ["here"])  (Dictation ["there"])  (Dictation ["bike"])
goodDirectionsF = DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss <> map (x:) xss
 where xss = powerset xs

inputDirectionsF  = map (intercalate " ") . map ("directions":) . powerset $ ["by bike","to there","from here"]
outputDirectionsF =
 [ DirectionsF Nothing               Nothing                Nothing
 , DirectionsF (Just (Place "here")) Nothing                Nothing
 , DirectionsF Nothing               (Just (Place "there")) Nothing
 , DirectionsF (Just (Place "here")) (Just (Place "there")) Nothing
 , DirectionsF Nothing               Nothing                (Just Bike)
 , DirectionsF (Just (Place "here")) Nothing                (Just Bike)
 , DirectionsF Nothing               (Just (Place "there")) (Just Bike)
 , DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)
 ]


handleParse action = (print =<< action) `catches`
 [ handler _ParseError print
 ]

-- |
-- catching :: MonadCatch m => Getting (First a) SomeException a          -> m r   -> (a -> m r)            -> m r
-- catching :: MonadCatch m =>            Prism' SomeException a          -> m r   -> (a -> m r)            -> m r
-- catching ::                            Prism' SomeException ParseError -> IO () -> (ParseError -> IO ()) -> IO ()
main :: IO ()
main = do

 putStrLn ""
 --   handling ERROR with HANDLER run ACTION
 handleParse (command `parses` "unknown-command")
 putStrLn ""
 handling _ParseError print (print =<< command `parses` "replace this no-with")
 putStrLn ""
 (print =<< command `parses` "replace with dictation-can't-have-zero-words") `catch` (\(e :: ParseError) -> print e)

 putStrLn ""
 putStrLn "free directions: "
 print . all (== Just (goodDirectionsF)) . map (directionsF `parses`) $ exampleDirections
 putStrLn "free directions: "
 print $ outputDirectionsF == ((directionsF `parses`) =<< inputDirectionsF)
 putStrLn "sensitive directions: "
 print . all (== (Just goodDirections))  . map (directionsS `parses`) $ exampleDirections
 _ <- traverse print $ map ((directionsS `parses`) :: String -> Maybe DirectionsS) exampleDirections
 putStrLn "multiword sensitive directions: "
 print =<< directionsS `parses` "directions to San Francisco from Redwood City by the bike"

 putStrLn ""
 print =<< test `parses` "test test undo"
 print =<< test `parses` "test test replace this and that with that and this"

 putStrLn ""
 print =<< command `parses` "undo"
 print =<< command `parses` "replace this and that with that and this"

 print =<< command `parses` "3 undo"
