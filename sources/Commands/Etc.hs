{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Commands.Etc where
import Commands.Instances  ()
import Control.Lens        (Prism', prism)
import Control.Monad.Catch (MonadThrow, SomeException (..))
import Data.Typeable       (cast)
import Text.Parsec         (ParseError)


-- | generalized 'Maybe':
--
-- >>> (return "actually" :: Possibly String) :: Maybe String
-- Just "actually"
--
-- >>> (return "actually" :: Possibly String) :: [String]
-- ["actually"]
--
-- >>> import Control.Exception
-- >>> (return "actually" :: Possibly String) :: Either SomeException String
-- Right "actually"
--
--
type Possibly a = (MonadThrow m) => m a

-- | existentially-quantify any unary type-constructor
--
--
data Some f = forall x. Some (f x)

-- | The constructors of a (zero-based) Enum.
--
-- >>> constructors :: [Bool]
-- [False,True]
--
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

_ParseError :: Prism' SomeException ParseError
_ParseError = prism SomeException $ \(SomeException e) ->
 case cast e of
  Nothing -> Left (SomeException e)
  Just a  -> Right a

