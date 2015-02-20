{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Commands.Etc where
import Commands.Instances           ()
import Control.Applicative
import Control.Monad.Catch          (MonadThrow, throwM)
import Data.Monoid                  ((<>))
import Data.Text.Lazy               (Text)
import Language.Haskell.TH.Syntax   (ModName (ModName), Name (..),
                                     NameFlavour (NameG), OccName (OccName),
                                     PkgName (PkgName))
import Text.PrettyPrint.Leijen.Text (Doc, displayT, renderPretty)


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

failed :: String -> Possibly a
failed = throwM . userError


-- | easily define smart constructors, whose error message has a
-- fully-qualified name for debugging. if you rename the module, the
-- error message changes automatically. and if you rename the
-- identifier, you will get a compile time error from Template Haskell
-- if you don't update the error message
-- (unless another name is captured).
--
-- e.g.
--
-- @
-- natural :: Integer -> Possibly Natural
-- natural i
--  | i >= 0    = return $ Natural i
--  | otherwise = failure 'natural
-- @
--
failure :: Name -> Possibly a
failure = throwM . userError . showName

showName :: Name -> String
showName = either show showGUI . fromName

-- | only 'NameG' is global.
fromName :: Name -> Possibly GUI
fromName (Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod))) = return $ GUI (Package pkg) (Module mod) (Identifier occ)
fromName (Name (OccName occ) _) = failed occ

-- | >>> showGUI (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier"))
-- "package-Module.SubModule.identifier"
showGUI :: GUI -> String
showGUI (GUI (Package pkg) (Module mod) (Identifier occ)) = pkg <> "-" <> mod <> "." <> occ

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

newtype Package    = Package    String deriving (Show, Eq, Ord)
newtype Module     = Module     String deriving (Show, Eq, Ord)
newtype Identifier = Identifier String deriving (Show, Eq, Ord)


-- | should have four field: @Version@.
data GUI = GUI !Package !Module !Identifier deriving (Show, Eq, Ord)

display :: Doc -> Text
display = displayT . renderPretty 1.0 80

-- | logical implication as Boolean propositions. makes reading validators easier. read @p --> q@ it as "p implies q".
(-->) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p --> q = ((||) <$> (not . p) <*> q)
