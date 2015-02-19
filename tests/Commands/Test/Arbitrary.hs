{-# LANGUAGE DataKinds, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Test.Arbitrary where
import Commands.Etc
import Commands.Test.Types
import Commands.Test.Etc
import Test.QuickCheck
import Commands.Frontends.Dragon13.Types
import           Commands.Frontends.Dragon13.Text
import           Data.Text.Lazy      (Text)
import qualified           Data.Text.Lazy as T
import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Exts (IsList (..))
import Debug.Trace (traceShowId)


instance (Arbitrary n, Arbitrary t) => Arbitrary (DNSGrammar n t) where
 arbitrary = DNSGrammar <$> arbitrary <*> resized 2 arbitrary

instance (Arbitrary n, Arbitrary t) => Arbitrary (DNSProduction True n t) where
 arbitrary = DNSProduction <$> arbitrary <*> resized 2 arbitrary
instance (Arbitrary n, Arbitrary t) => Arbitrary (DNSProduction False n t) where
 arbitrary = oneof
  [ DNSImport     <$> arbitrary

  , DNSProduction <$> arbitrary <*> resized 2 arbitrary
  , DNSVocabulary <$> arbitrary <*> resized 2 arbitrary
  ]

instance (Arbitrary n, Arbitrary t) => Arbitrary (DNSRHS n t) where
 arbitrary = oneof
  [ DNSTerminal     <$> arbitrary
  , DNSNonTerminal  <$> (arbitrary :: Gen (DNSLHS LHSList n)) -- avoids @OverlappingInstances@
  , DNSNonTerminal  <$> (arbitrary :: Gen (DNSLHS LHSRule n))

  , DNSOptional     <$> resized 2 arbitrary
  , DNSMultiple     <$> resized 2 arbitrary

  , DNSSequence     <$> resized 2 arbitrary
  , DNSAlternatives <$> resized 2 arbitrary
  ]

instance (Arbitrary n) => Arbitrary (DNSLHS LHSList n) where arbitrary = DNSList <$> arbitrary
instance (Arbitrary n) => Arbitrary (DNSLHS LHSRule n) where
 arbitrary = oneof
  [ DNSRule    <$> arbitrary
  , DNSBuiltin <$> arbitrary
  ]

instance (Arbitrary t) => Arbitrary (DNSToken t) where
 arbitrary = oneof
  [ DNSToken      <$> arbitrary
  , DNSPronounced <$> arbitrary <*> arbitrary
  ]

instance Arbitrary DNSBuiltin where arbitrary = elements constructors

instance Arbitrary Text where arbitrary = T.pack <$> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where arbitrary = (:|) <$> arbitrary <*> arbitrary

-- not constrained enough: "*** Gave up! Passed only 3 tests."
instance Arbitrary TinyText where
 arbitrary = do
  n <- choose (0,3)
  s <- resize n arbitrary
  return $ TinyText s

instance Arbitrary DNSText where
 arbitrary = do
  n <- choose (1,3)
  let s = T.pack <$> resize n arbitrary
  DNSText <$> (s `suchThat` isDNSText)

instance Arbitrary DNSName where
 arbitrary = do
  n <- choose (1,3)
  let s = T.pack <$> resize n arbitrary
  DNSName <$> (s `suchThat` isDNSName)
