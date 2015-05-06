{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module Commands.Mixins.DNS13OSX9.Types where
import Commands.Grammar.Types
import Commands.Mixins.DNS13.Types
import Commands.Backends.OSX.Types
import Commands.Parsers.Earley.Types


type C = Command EarleyParser DNSReifying ApplicationDesugarer

type G = Grammar EarleyParser DNSReifying

type R = RHS     EarleyParser DNSReifying
