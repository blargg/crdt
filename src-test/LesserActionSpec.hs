{-# LANGUAGE ScopedTypeVariables #-}
module LesserActionSpec where

import Algebra.LesserAction
import Algebra.SemiLatticeAction
import Data.Set
import Data.Proxy
import Data.ChatData
import Data.Map (Map)

import Test.Tasty
import Test.Tasty.QuickCheck

allLesserAction :: TestTree
allLesserAction = testGroup "LesserAction"
    [ setIsJoinLesserAction
    , chatDataLesserAction
    , specJoinLesserAction (Proxy :: Proxy (Map Int (Set Int))) (Proxy :: Proxy (Map Int (Set Int))) "Map Int (Set Int) is LesserAction"
    ]
applicationOfLesserAction :: (Eq a, JoinLesserAction s a) => s -> a -> Bool
applicationOfLesserAction action state = joinLesserEq action state == (apply action state == state)

specJoinLesserAction :: forall s a. (JoinLesserAction s a, Arbitrary s, Arbitrary a, Eq a, Show s, Show a) =>
                         Proxy s -> Proxy a -> String -> TestTree
specJoinLesserAction _ _ name = testGroup name
    [ testProperty "is lesser action iff no change" (applicationOfLesserAction :: s -> a -> Bool)]

setIsJoinLesserAction :: TestTree
setIsJoinLesserAction = testGroup "set is JoinLesserAction"
    [ testProperty "joinLesserEq action state == (apply action state == state)" (applicationOfLesserAction:: Set Int -> Set Int -> Bool)]

chatDataLesserAction :: TestTree
chatDataLesserAction =
    specJoinLesserAction (Proxy :: Proxy (ChatData String)) (Proxy :: Proxy (ChatData String)) "ChatData is JoinLesserAction"
