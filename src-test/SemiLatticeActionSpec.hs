{-# LANGUAGE ScopedTypeVariables #-}
module SemiLatticeActionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Algebra.SemiLatticeAction
import Data.Set
import Algebra.Lattice
import Data.Proxy
import Data.ChatData
import Data.Map (Map)

allSemiLatticeAction :: TestTree
allSemiLatticeAction = testGroup "SemiLatticeAction"
    [ setIsSemiLatticeAction
    , chatDataSemiLatticeAction
    , specSemiLatticeAction (Proxy :: Proxy (Map Int (Set Int))) (Proxy :: Proxy (Map Int (Set Int))) "Map Int is SemiLatticeAction"
    ]
applyActsOnType :: (JoinSemiLatticeAction s a, Eq a) => s -> s -> a -> Bool
applyActsOnType s t x = apply (s \/ t) x == apply s (apply t x)

specSemiLatticeAction :: forall s a. (JoinSemiLatticeAction s a, Arbitrary s, Arbitrary a, Eq a, Show s, Show a) =>
                         Proxy s -> Proxy a -> String -> TestTree
specSemiLatticeAction _ _ name = testGroup name
    [ testProperty "apply is an action" (applyActsOnType :: s -> s -> a -> Bool)]

setIsSemiLatticeAction :: TestTree
setIsSemiLatticeAction = testGroup "set is SemiLatticeAction"
    [ testProperty "apply is an action" (applyActsOnType :: Set Int -> Set Int -> Set Int -> Bool)
    ]

chatDataSemiLatticeAction :: TestTree
chatDataSemiLatticeAction =
    specSemiLatticeAction (Proxy :: Proxy (ChatData String)) (Proxy :: Proxy (ChatData String)) "ChatData is JoinSemiLatticeAction"
