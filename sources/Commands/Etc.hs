{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Commands.Etc where
import Commands.Instances()
import Control.Monad.Catch (MonadThrow)


-- | existentially-quantify any unary type-constructor
data Some f = forall x. Some (f x)

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
