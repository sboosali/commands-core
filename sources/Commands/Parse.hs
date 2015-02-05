{-# LANGUAGE RankNTypes #-}
module Commands.Parse where
import Commands.Etc
import Commands.Munging      (unCamelCase)
import Commands.Parse.Types
import Commands.Parsec
import Control.Applicative
import Control.Concatenative (bi)
import Data.Foldable         (asum)
import Data.List             (intercalate)
import Data.Monoid           (First (..))


-- (&) = (<*>)

-- (#) = (<$>)

terminal :: String -> SensitiveParser String
terminal = freely . word

parses :: SensitiveParser a -> String -> Possibly a
parses (SensitiveParser _ p) = parse $ p (Some eof)

contextual :: Parsec x -> First Context
contextual = First . Just . Some

freely :: Parsec a -> SensitiveParser a
freely = bi contextual const SensitiveParser

con :: (Show a) => a -> SensitiveParser a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> SensitiveParser Int
int = con

twig :: (Enum a, Show a) => SensitiveParser a
twig = asum . map con $ constructors
