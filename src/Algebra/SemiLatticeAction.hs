{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.SemiLatticeAction where

import           Algebra.Lattice
import Algebra.Lattice.Ordered
import Data.Map (Map)
import Data.Set (Set)

-- |
-- Class where SemiLattice @s@ acts on @a@
-- follows the law
-- * @apply (s1 \/ s1) = apply s1 . apply s2@
class (JoinSemiLattice s) => JoinSemiLatticeAction s a where
    apply :: s -> a -> a

instance (Ord s) => JoinSemiLatticeAction (Ordered s) (Ordered s) where
    apply s a = s \/ a

instance (Ord k, JoinSemiLattice a) => JoinSemiLatticeAction (Map k a) (Map k a) where
    apply s a = s \/ a

instance (JoinSemiLatticeAction s a, JoinSemiLatticeAction t b) => JoinSemiLatticeAction (s, t) (a, b) where
    apply (s1, s2) (a1, a2) = (apply s1 a1, apply s2 a2)

instance (Ord a) => JoinSemiLatticeAction (Set a) (Set a) where
    apply s a = s \/ a
