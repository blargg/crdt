module LabeledSet where

import qualified Data.Map as M

data LabeledSet k a = LabeledSet { nextKey :: k
                                 , entries :: M.Map k a
                                 }

emptySet :: k -> LabeledSet k a
emptySet initialKey = LabeledSet initialKey M.empty

addNext :: (Ord k, Enum k) => a -> LabeledSet k a -> (LabeledSet k a, k)
addNext val (LabeledSet key m) = (LabeledSet (succ key) (M.insert key val m), key)

retreive :: (Ord k) => k -> LabeledSet k a -> Maybe a
retreive k (LabeledSet _ m) = M.lookup k m

filterWithLabel :: (k -> Bool) -> LabeledSet k a -> LabeledSet k a
filterWithLabel f (LabeledSet nk ents) = LabeledSet nk (M.filterWithKey (\k _ -> f k) ents)

toList :: LabeledSet k a -> [(k, a)]
toList = M.toList . entries
