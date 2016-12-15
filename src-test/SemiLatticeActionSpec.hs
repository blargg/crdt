module SemiLatticeActionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Algebra.SemiLatticeAction
import Data.Set
import Algebra.Lattice

applyActsOnType :: (JoinSemiLatticeAction s a, Eq a) => s -> s -> a -> Bool
applyActsOnType s t x = apply (s \/ t) x == apply s (apply t x)

setIsSemiLatticeAction :: TestTree
setIsSemiLatticeAction = testGroup "set is SemiLatticeAction"
    [ testProperty "apply is an action" (applyActsOnType :: Set Int -> Set Int -> Set Int -> Bool)
    ]
