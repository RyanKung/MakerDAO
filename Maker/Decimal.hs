{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}



module Maker.Decimal (Decimal (..), E18, E36, Ε(..)) where
import Prelude ()
import Maker.Prelude
import Data.Fixed

newtype HasResolution e ⇒ Decimal e = D (Fixed e)
  deriving (Ord, Eq, Real, RealFrac)


-- instance HasResolution e ⇒ Read (Decimal e) where
--   readsPrec n s = fmap (\(x, y) → (D, x, y)) (readsPrec n s)

instance HasResolution e ⇒ Show (Decimal e) where
  show (D x) = show x

instance HasResolution e ⇒ Num (Decimal e) where
  x@(D (MkFixed a)) * D (MkFixed b) =
    D (MkFixed (div (a * b + div (resolution x) 2)
               (resolution x)))
  D a + D b = D (a + b)
  D a - D b = D (a - b)
  negate (D a) = D (negate a)
  abs (D a) = D (abs a)
  signum (D a) = D (signum a)
  fromInteger i = D (fromInteger i)

instance HasResolution e ⇒ Fractional (Decimal e) where
  x@(D (MkFixed a)) / D(MkFixed b) =
    D (MkFixed (div (a * resolution x + div b 2) b))
  recip (D a) = D (recip a)
  fromRational r = D (fromRational r)

data E18; data E36
instance HasResolution E18 where
  resolution _ = 10 ^ (18 ∷ Integer)
instance HasResolution E36 where
    resolution _ = 10 ^ (36 ∷ Integer)


class Εpsilon t where (∉) ∷ t
instance HasResolution a ⇒ Εpsilon (Decimal a) where
  (∉) = 1 / fromIntegral (resolution (undefined::Fixed a))
