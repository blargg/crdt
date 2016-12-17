{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.SemiLatticeAction where

import           Algebra.Lattice

-- |
-- Class where SemiLattice @s@ acts on @a@
-- follows the law
-- * @apply (s1 \/ s1) = apply s1 . apply s2@
class (JoinSemiLattice s) => JoinSemiLatticeAction s a where
    apply :: s -> a -> a

-- |
-- When type @a@ is acting on type @a@, use the join action
instance (JoinSemiLattice a) => JoinSemiLatticeAction a a where
    apply = (\/)