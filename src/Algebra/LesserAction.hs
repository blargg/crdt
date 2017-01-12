{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Algebra.LesserAction where


import           Algebra.Lattice.Ordered
import           Algebra.SemiLatticeAction
import           Data.Map                  (Map)
import qualified Data.Map                  as M
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

instance (JoinLesserAction s a, JoinLesserAction t b) => JoinLesserAction (s, t) (a, b) where
    (s1, s2) `joinLesserEq` (a1, a2) = left && right
        where left = s1 `joinLesserEq` a1
              right = s2 `joinLesserEq` a2

instance (Ord k, JoinSemiLatticeAction (Map k a) (Map k b), JoinLesserAction a b) => JoinLesserAction (Map k a) (Map k b) where
    s `joinLesserEq` x = subset && allLesser
        where subset = S.isSubsetOf (M.keysSet s) (M.keysSet x)
              allLesser = and $ M.intersectionWith joinLesserEq s x
