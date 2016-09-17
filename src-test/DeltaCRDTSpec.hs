{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaCRDTSpec where

import DeltaCRDT
import Data.Set

import Test.Tasty
import Test.Tasty.QuickCheck

applyIsIdempotent :: (Eq a, DCRDT a) => a -> Delta a -> Bool
applyIsIdempotent x delta = apply delta (apply delta x) == apply delta x

applyCommutes :: (Eq a, DCRDT a) => a -> Delta a -> Delta a -> Bool
applyCommutes a d1 d2 = apply d2 (apply d1 a) == apply d1 (apply d2 a)

setIsDCRDT :: [TestTree]
setIsDCRDT =
    [ testProperty "Apply Idempotent" (applyIsIdempotent :: Set Int -> Delta (Set Int) -> Bool)
    , testProperty "Apply Commutes" (applyCommutes :: Set Int -> Delta (Set Int) -> Delta (Set Int) -> Bool)
    ]
