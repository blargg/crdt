module Data.MultiAckSet ( MultiAckSet()
                        , addNeighbor
                        , canMultiCast
                        , empty
                        , neighbors
                        , multiCast
                        , listMessages
                        , listTakeMessages
                        , setWithNeighbors
                        , singleAck
                        ) where

import           Data.AckSet   hiding (empty)
import qualified Data.AckSet   as AS
import qualified Data.Map      as M
import qualified Data.Set      as S
import           Data.Universe

newtype MultiAckSet n i a = MultiAckSet { toMap :: M.Map n (AckSet i a)}

empty :: MultiAckSet n i a
empty = MultiAckSet M.empty

setWithNeighbors :: (Ord n, Universe i) => [n] -> MultiAckSet n i a
setWithNeighbors = foldl (flip addNeighbor) empty

addNeighbor :: (Ord n, Universe i)  => n -> MultiAckSet n i a -> MultiAckSet n i a
addNeighbor n = MultiAckSet . M.insertWith (flip const) n AS.empty . toMap

neighbors :: MultiAckSet n i a -> S.Set n
neighbors = M.keysSet . toMap

multiCast :: (Ord i) => a -> MultiAckSet n i a -> Maybe (MultiAckSet n i a)
multiCast x (MultiAckSet m) = MultiAckSet <$> traverse (insert x) m

canMultiCast :: MultiAckSet n i a -> Bool
canMultiCast (MultiAckSet m) = all canInsert m

listMessages :: MultiAckSet n i a -> [(n, i, a)]
listMessages mack = do
    (node, ack) <- M.toList . toMap $ mack
    (index, x) <- getUnacked ack
    return (node, index, x)

listTakeMessages :: Int -> MultiAckSet n i a -> [(n, i, a)]
listTakeMessages amount mack = do
    (node, ack) <- M.toList . toMap $ mack
    (index, x) <- take amount $ getUnacked ack
    return (node, index, x)

singleAck :: (Ord n, Ord i) => n -> i -> MultiAckSet n i a -> MultiAckSet n i a
singleAck node index (MultiAckSet m) = MultiAckSet m'
    where
        m' = M.update (Just . acknowledge index) node m
