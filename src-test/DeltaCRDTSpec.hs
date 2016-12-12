{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaCRDTSpec where

import DeltaCRDT
import Data.Set
import Algebra.Lattice.Ordered

import Test.Tasty
import Test.Tasty.QuickCheck

applyIsIdempotent :: (Eq a, DCRDT a) => a -> Delta a -> Bool
applyIsIdempotent x delta = apply delta (apply delta x) == apply delta x

applyCommutes :: (Eq a, DCRDT a) => a -> Delta a -> Delta a -> Bool
applyCommutes a d1 d2 = apply d2 (apply d1 a) == apply d1 (apply d2 a)

idempotentCheck :: (DCRDT' a, Eq a) => Delta a -> a -> Bool
idempotentCheck d x = isIdempotent d x == (apply d x == x)

setIsDCRDT :: TestTree
setIsDCRDT = testGroup "set is DCRDT"
    [ testProperty "Apply Idempotent" (applyIsIdempotent :: Set Int -> Delta (Set Int) -> Bool)
    , testProperty "Apply Commutes" (applyCommutes :: Set Int -> Delta (Set Int) -> Delta (Set Int) -> Bool)
    ]

setIsDCRDT' :: TestTree
setIsDCRDT' = testGroup "set is DCRDT'"
    [ setIsDCRDT
    , testProperty "isIdempotent iff apply delta x == x" (idempotentCheck :: Delta (Set Int) -> Set Int -> Bool)
    ]

newtype OrdInt = OrdInt { getOrdInt :: Ordered Int }
    deriving (Show)

instance Arbitrary OrdInt where
    arbitrary = OrdInt . Algebra.Lattice.Ordered.Ordered <$> arbitrary

orderedIntApply :: OrdInt -> Delta (Ordered Int) -> Bool
orderedIntApply (OrdInt x) = applyIsIdempotent x

orderedIntComm :: OrdInt -> Delta (Ordered Int) -> Delta (Ordered Int) -> Bool
orderedIntComm (OrdInt x) = applyCommutes x

orderedIntIdempotent :: Delta (Ordered Int) -> OrdInt -> Bool
orderedIntIdempotent delta (OrdInt x) = idempotentCheck delta x

orderedIntIsDCRDT :: TestTree
orderedIntIsDCRDT = testGroup "Ordered Int is DCRDT"
    [ testProperty "Apply Idempotent" orderedIntApply
    , testProperty "Apply Commutes" orderedIntComm
    ]

orderedIntIsDCRDT' :: TestTree
orderedIntIsDCRDT' = testGroup "Ordered Int is DCRDT'"
    [ setIsDCRDT
    , testProperty "isIdempotent iff apply delta x == x" orderedIntIdempotent
    ]
