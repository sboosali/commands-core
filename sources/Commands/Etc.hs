{-# LANGUAGE ExistentialQuantification #-}
module Commands.Etc where


-- | existentially-quantify any unary type-constructor
data Some f = forall x. Some (f x)
