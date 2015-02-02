{-# LANGUAGE ExistentialQuantification, RankNTypes, GADTs #-}
module Commands.Etc where
import Commands.Instances()

import Control.Monad.Catch (MonadThrow)

import Data.Typeable (Typeable,tyConPackage,tyConModule,tyConName,typeRepTyCon,typeRep)


-- | existentially-quantify any unary type-constructor
data Some f = forall x. Some (f x)

-- | existentially-quantify any unary type-constructor, whose "type
-- argument" is constrained to be Typeable, to gain back some of the
-- info lost.
-- 
-- 
data Exists f where
 Exists :: forall f x. (Typeable x) => f x -> Exists f

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

-- | The constructors of a (zero-based) Enum.
-- 
-- >>> constructors :: [Bool]
-- [False,True]
-- 
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

type Package    = String
type Module     = String
type Identifier = String

-- | the globally unique identifier of a type: @(pkg,
-- <https://www.haskell.org/onlinereport/lexemes.html modid>,
-- <https://www.haskell.org/onlinereport/lexemes.html tycon>)@
-- 
-- 
guiOf :: (Typeable a) => proxy a -> (Package, Module, Identifier)
guiOf = (\t -> (tyConPackage t, tyConModule t, tyConName t)) . typeRepTyCon . typeRep
