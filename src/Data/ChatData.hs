{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.ChatData ( ChatData(..)
                     , addMessageDelta
                     , empty) where

import           Algebra.Lattice
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Serialize
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

import           Algebra.LesserAction
import           Algebra.SemiLatticeAction

data ChatData a = ChatData { lamportTime :: Int
                           , messages    :: Map Int (Set a) }
                           deriving (Show, Read, Eq, Generic)

instance (Ord a, Serialize a) => Serialize (ChatData a)

instance (Ord a) => JoinSemiLattice (ChatData a) where
    (ChatData time1 messages1) \/ (ChatData time2 messages2) = ChatData (max time1 time2) (messages1 \/ messages2)

instance (Ord a) => JoinSemiLatticeAction (ChatData a) (ChatData a) where
    apply s a = s \/ a

instance (Ord a) => JoinLesserAction (ChatData a) (ChatData a) where
    (ChatData time1 messages1) `joinLesserEq` (ChatData time2 messages2) = lessTime && lessMessages
        where lessTime = time1 <= time2
              lessMessages = messages1 `joinLesserEq` messages2

instance (Arbitrary a, Ord a) => Arbitrary (ChatData a) where
    arbitrary = ChatData <$> arbitrary <*> arbitrary

empty :: ChatData a
empty = ChatData 0 M.empty

addMessageDelta :: a -> ChatData a -> ChatData a
addMessageDelta x (ChatData time _) = ChatData time' (M.singleton time' (S.singleton x))
    where time' = time + 1
