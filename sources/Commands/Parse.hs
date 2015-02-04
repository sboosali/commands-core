{-# LANGUAGE RankNTypes #-}
module Commands.Parse where
import Commands.Etc
import Commands.Munging     (unCamelCase)
import Commands.Parse.Types
import Commands.Parsec
import Control.Applicative
import Data.Foldable        (asum)
import Data.List            (intercalate)


-- (&) = (<*>)

-- (#) = (<$>)

terminal :: String -> SensitiveParser String
terminal s = SensitiveParser $ \_ -> word s

parses :: SensitiveParser a -> String -> Possibly a
parses (SensitiveParser sp) = parse $ sp (Some eof)

freely :: Parsec a -> SensitiveParser a
freely = SensitiveParser . const

con :: (Show a) => a -> SensitiveParser a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> SensitiveParser Int
int = con

twig :: (Enum a, Show a) => SensitiveParser a
twig = asum . map con $ constructors
