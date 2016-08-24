{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module DeltaCRDT where

import qualified Data.Set as S
import GHC.Generics
import Data.TotalMap

import Test.QuickCheck.Arbitrary

class DCRDT a where
    data Delta a :: *
    apply :: Delta a -> a -> a
    merge :: Delta a -> Delta a -> Delta a


instance (Ord a) => DCRDT (S.Set a) where
    data Delta (S.Set a) = DeltaSet (S.Set a) deriving (Show, Eq, Generic)
    apply (DeltaSet d) x = d `S.union` x
    merge (DeltaSet d) (DeltaSet d2) = DeltaSet (S.union d d2)

instance (Ord a, Arbitrary a) => Arbitrary (Delta (S.Set a)) where
    arbitrary = DeltaSet . S.fromList <$> arbitrary


instance (Ord k, DCRDT a) => DCRDT (TotalMap k a) where
    data Delta (TotalMap k a) = DeltaTotalMap (TotalMap k (Delta a))
    apply (DeltaTotalMap d) x = apply <$> d <*> x
    merge (DeltaTotalMap d1) (DeltaTotalMap d2) = DeltaTotalMap $ merge <$> d1 <*> d2
