module Commands.Parsers.Earley where
import Commands.Parsers.Earley.Types
import Commands.Grammar.Types
import Control.Alternative.Free.Tree

import qualified Text.Earley as E



induceEarleyParser :: RHS a -> EarleyParser r a
induceEarleyParser = runAlt $ symbol fromWord fromRule
 where
 fromWord s = E.symbol s *> pure undefined
 fromRule g = (g^.gramParser) E.<?> showLHS (g^.gramLHS) sp context
