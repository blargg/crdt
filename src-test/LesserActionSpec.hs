module LesserActionSpec where

import Algebra.LesserAction
import Algebra.SemiLatticeAction
import Data.Set

import Test.Tasty
import Test.Tasty.QuickCheck

applicationOfLesserAction :: (Eq a, JoinLesserAction s a) => s -> a -> Bool
applicationOfLesserAction action state = joinLesserEq action state == (apply action state == state)

setIsJoinLesserAction :: TestTree
setIsJoinLesserAction = testGroup "set is JoinLesserAction"
    [ testProperty "joinLesserEq action state == (apply action state == state)" (applicationOfLesserAction:: Set Int -> Set Int -> Bool)]
