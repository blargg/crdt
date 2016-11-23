{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module DeltaCRDT where

import qualified Data.Set as S
import GHC.Generics
import Data.TotalMap
import Data.Serialize
import Algebra.Lattice
import Algebra.Lattice.Ordered

import Test.QuickCheck.Arbitrary

class DCRDT a where
    data Delta a :: *
    apply :: Delta a -> a -> a

class (DCRDT a) => DCRDT' a where
    isIdempotent :: Delta a -> a -> Bool

instance (Ord a) => DCRDT (Ordered a) where
    data Delta (Ordered a) = DeltaOrdered a deriving (Show, Generic)
    apply (DeltaOrdered d) a = Ordered d \/ a

instance (Ord a) => DCRDT' (Ordered a) where
    isIdempotent (DeltaOrdered d) (Ordered a) = d <= a

instance (Serialize a) => Serialize (Delta (Ordered a))

instance Arbitrary a => Arbitrary (Delta (Ordered a)) where
    arbitrary = DeltaOrdered <$> arbitrary

instance (Ord a) => DCRDT (S.Set a) where
    data Delta (S.Set a) = DeltaSet (S.Set a) deriving (Show, Eq, Generic)
    apply (DeltaSet d) x = d `S.union` x

instance (Ord a) => DCRDT' (S.Set a) where
    isIdempotent (DeltaSet d) = S.isSubsetOf d

instance (Ord a, Arbitrary a) => Arbitrary (Delta (S.Set a)) where
    arbitrary = DeltaSet . S.fromList <$> arbitrary


instance (Ord k, DCRDT a) => DCRDT (TotalMap k a) where
    data Delta (TotalMap k a) = DeltaTotalMap (TotalMap k (Delta a))
    apply (DeltaTotalMap d) x = apply <$> d <*> x
