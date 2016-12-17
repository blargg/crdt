{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.LesserAction where


import           Algebra.Lattice.Ordered
import           Algebra.SemiLatticeAction
import           Data.Set                  (Set)
import qualified Data.Set                  as S

-- |
-- Check if applying the semilattice value would have any affect
-- the name comes from the observation @s1,s2::s, x:: a, if s1 <= s2 then s1 `lesserThan` (apply s2 x)@
-- * @s `joinLesserEq` a = (apply s a == a)@
class JoinSemiLatticeAction s a => JoinLesserAction s a where
    joinLesserEq :: s -> a -> Bool


instance (Ord a) => JoinLesserAction (Ordered a) (Ordered a) where
    action `joinLesserEq` state = action <= state

instance (Ord a) => JoinLesserAction (Set a) (Set a) where
    action `joinLesserEq` state = action `S.isSubsetOf` state
