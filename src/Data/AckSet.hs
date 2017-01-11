module Data.AckSet ( AckSet()
                   , acknowledge
                   , canInsert
                   , empty
                   , getUnacked
                   , insert
                   ) where

import qualified Data.Map as M
import Data.Universe

data AckSet i a = AckSet [i] (M.Map i a)

empty :: (Universe i) => AckSet i a
empty = AckSet universe M.empty

-- we should probably handle the error more elegantly
-- concidering changing this to a maybe type
insert :: (Ord i) => a -> AckSet i a -> Maybe (AckSet i a)
insert _ (AckSet [] _) = Nothing
insert x (AckSet (nextId:remainingIds) m) = Just $ AckSet remainingIds m'
    where m' = M.insert nextId x m

acknowledge :: (Ord i) => i -> AckSet i a -> AckSet i a
acknowledge identifier (AckSet nextId m) = AckSet nextId (M.delete identifier m)

getUnacked :: AckSet i a -> [(i,a)]
getUnacked (AckSet _ m)= M.toList m

-- not (canInsert ack) <=> (insert a ack == Nothing)
canInsert :: AckSet i a -> Bool
canInsert (AckSet [] _) = False
canInsert _             = True
