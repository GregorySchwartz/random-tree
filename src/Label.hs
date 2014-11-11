-- Label module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the labeling of a tree

module Label where

-- Built-in
import Data.List
import Data.Maybe
import Data.Tree
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- Cabal
import Control.Monad.Random
import System.Random.Shuffle

-- Local
import Types

-- | Modify the label of a tree
modifyLabel :: (Eq a) => a -> a -> a -> a
modifyLabel old new n = if n == old
                            then new
                            else n

-- | Relabel the tree
relabelTree :: (Eq a) => LabelMap a -> Tree a -> Tree a
relabelTree labelMap tree =
    foldl' (\accTree (x, y) -> fmap (modifyLabel x y) accTree) tree
  $ M.toAscList labelMap

-- | Assign clumps to the label list. Takes an old label and reassigns it to the
-- new label in the labelmap, but looks at all neighbors defined by the
-- distanceMap and the neighborDistance. If the reassigned nodes have already
-- been reassigned (in the labelList), then ignore.
clumpIt :: (Eq a, Ord a)
        => LabelList a
        -> Int
        -> DistanceMap a
        -> a
        -> a
        -> LabelMap a
        -> LabelMap a
clumpIt labelList neighborDistance distanceMap old new labelMap =
    F.foldl' (\acc x -> updateMap x new acc) labelMap $ neighbors old
  where
    updateMap k v = M.update
                    (\x -> Just $ if x `elem` labelList then x else v)
                    k
    neighbors x   = S.take neighborDistance
                  . F.foldl' (S.><) S.empty
                  . map snd
                  . M.toAscList
                  . fromJust
                  $ M.lookup x distanceMap

-- | Assign random labels to the leaves of a tree in a clumped fashion
assignRandomClumpedLabels :: (Eq a, Ord a)
                          => LabelList a
                          -> Int
                          -> DistanceMap a
                          -> StdGen
                          -> LabelMap a
                          -> LabelMap a
assignRandomClumpedLabels labelList neighborDistance distanceMap g labelMap =
    foldl' ( \acc (x, y)
          -> clumpIt labelList neighborDistance distanceMap x y acc)
    labelMap
  . zip shuffledLeaves
  $ labelList
  where
    shuffledLeaves = shuffle' (M.keys labelMap) (M.size labelMap) g

-- | Assign random labels to the leaves of a tree
assignRandomLabels :: (Eq a, Ord a)
                   => LabelList a
                   -> StdGen
                   -> LabelMap a
                   -> LabelMap a
assignRandomLabels labelList g labelMap = M.fromList
                                        . zip shuffledLeaves
                                        $ labelList
  where
    shuffledLeaves = shuffle' (M.keys labelMap) (M.size labelMap) g

-- | Return the labelMap
getLabelMap :: (Ord a) => [a] -> LabelMap a
getLabelMap x = M.fromList . zip x $ x
