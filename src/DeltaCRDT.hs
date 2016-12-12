{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module DeltaCRDT where

import qualified Data.Set as S
import GHC.Generics
import Data.TotalMap
import Data.Serialize
import Data.Universe
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

instance (Ord a, Serialize a) => Serialize (Delta (S.Set a))

instance (Ord a, Arbitrary a) => Arbitrary (Delta (S.Set a)) where
    arbitrary = DeltaSet . S.fromList <$> arbitrary


instance (Ord k, DCRDT a) => DCRDT (TotalMap k a) where
    data Delta (TotalMap k a) = DeltaTotalMap (TotalMap k (Delta a))
        deriving (Generic)
    apply (DeltaTotalMap d) x = apply <$> d <*> x

instance (Universe k, Ord k, DCRDT' a) => DCRDT' (TotalMap k a) where
    isIdempotent (DeltaTotalMap dm) tm = foldJoinSemiLattice True isIdemMap
        where isIdemMap = isIdempotent <$> dm <*> tm

instance (Ord k, Serialize k, DCRDT a, Serialize (Delta a)) => Serialize (Delta (TotalMap k a))
