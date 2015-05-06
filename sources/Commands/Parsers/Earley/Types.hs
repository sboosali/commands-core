module Commands.Parsers.Earley.Types where

import qualified Text.Earley as E


type EarleyParser z a = E.Prod z String String a
-- type Production a = forall z. E.Prod z String String a
