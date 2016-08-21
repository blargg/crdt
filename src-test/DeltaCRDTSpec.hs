{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaCRDTSpec where

import DeltaCRDT
import Data.Set
import Algebra.PartialOrd

import Test.Tasty
import Test.Tasty.QuickCheck



-- Applying a patch always moves up in the order.
-- Implies that there was some value y such that
-- `apply delta x == x /\ y`
advances :: (PartialOrd a, DCRDT a) => a -> Delta a -> Bool
advances x delta = x `leq` apply delta x

setIsDCRDT :: [TestTree]
setIsDCRDT =
    [ testProperty "Advances" (advances :: Set Int -> Delta (Set Int) -> Bool)
    , testProperty "Apply Idempotent" (applyIsIdempotent :: Set Int -> Delta (Set Int) -> Bool)
    , testProperty "Deltas Merge" (deltaMerge :: Set Int -> Delta (Set Int) -> Delta (Set Int) -> Bool)
    , testProperty "Merge Commutes" (mergeCommutes :: Set Int -> Delta (Set Int) -> Delta (Set Int) -> Bool)
    ]


applyIsIdempotent :: (Eq a, DCRDT a) => a -> Delta a -> Bool
applyIsIdempotent x delta = apply delta (apply delta x) == apply delta x

-- applying merged patches is the same as applying them seperately
deltaMerge :: (Eq a, DCRDT a) => a -> Delta a -> Delta a -> Bool
deltaMerge x d1 d2 = apply (merge d1 d2) x == apply d2 (apply d1 x)

-- Deltas can be merged in any order and produce the same result
mergeCommutes :: (Eq a, DCRDT a) => a -> Delta a -> Delta a -> Bool
mergeCommutes x d1 d2 = apply (merge d1 d2) x == apply (merge d2 d1) x
