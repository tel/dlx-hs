{-# LANGUAGE FlexibleContexts #-}

module Algorithms.DLX where

import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Set (Set)
import qualified Data.Set as Set

x :: [[Int]]
x = [[1,2],[5,6],[4,5],[1,2,3],[3,4],[4,5],[1,3,5,6]]

toSparse :: [[Int]] -> IntMap IntSet
toSparse = IntMap.fromList . zip [1..] . map IntSet.fromList

solve :: MonadPlus m => IntMap IntSet -> m [Int]
solve s = maybe (return []) go (IntMap.minView s) where
  go (c, s') = do r <- (msum . fmap return . IntSet.toList) c
                  sol <- solve (removeViolationsOld r s')
                  return (r:sol)

-- This currently only removes columns which have been satisfied by
-- this choice of row. We need to also remove all rows which are no
-- longer useful given these columns are gone---in other words, every
-- value used in a removed column needs to be removed from every other
-- column.
removeViolationsOld :: Int -> IntMap IntSet -> IntMap IntSet
removeViolationsOld r s = IntMap.filter (not . IntSet.member r) s

-- Somehow this seems to "add one" to all of the row names
removeViolations :: Int -> IntMap IntSet -> IntMap IntSet
removeViolations r s =
  let (good, bad) = IntMap.partition (not . IntSet.member r) s
      defunctRows = Set.unions (map (Set.fromList . IntSet.elems) $ IntMap.elems bad)
  in fmap (IntSet.filter (not . flip Set.member defunctRows)) good
