-- Label module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the labeling of a tree

module RandomTree.Label where

-- Built-in
import Data.List
import Data.Tree
import qualified Data.Map as M
import qualified Data.Foldable as F

-- Cabal
import Control.Monad.Random
import System.Random.Shuffle
import FunTree.Types

-- | Modify the label of a tree
modifyLabel :: (Eq a) => a -> a -> a -> a
modifyLabel old new n = if n == old
                            then new
                            else n

-- | Get the neighbors of a label in a fast, theoretically efficient way
getNeighbors :: (Ord a) => Int -> a -> Tree (SuperNode a) -> [a]
getNeighbors neighborDistance l ( Node { rootLabel = SuperRoot
                                       , subForest = ts } ) =
    getNeighbors neighborDistance l
  . head
  . filter (M.member l . myLeaves . rootLabel)
  $ ts
getNeighbors neighborDistance l ( Node { rootLabel = SuperNode { myRootLabel = _
                                                               , myParent = p
                                                               , myLeaves = ls }
                                       , subForest = ts } )
    | M.size ls == neighborDistance && relevant =
        map fst . M.toAscList $ ls
    | M.size ls > neighborDistance && relevant  =
        getNeighbors neighborDistance l
      . head
      . filter (M.member l . myLeaves . rootLabel)
      $ ts
    | M.size ls < neighborDistance && relevant  =
        take neighborDistance
      . (:) l
      . filter (/= l)
      . map fst
      . M.toAscList
      . myLeaves
      $ p
    | otherwise                                 = []
  where
    relevant = M.member l ls

-- | Assign clumps to the label list. Takes an old label and reassigns it to the
-- new label in the labelmap, but looks at all neighbors defined by the
-- distanceMap and the neighborDistance. If the reassigned nodes have already
-- been reassigned (in the propertyList), then ignore.
clumpIt :: (Ord a, Eq b)
        => Int
        -> Tree (SuperNode a)
        -> a
        -> b
        -> PropertyMap a b
        -> PropertyMap a b
clumpIt neighborDistance tree pointer property propertyMap =
    F.foldl' (\acc x -> updateMap x property acc) propertyMap
  $ neighbors pointer
  where
    updateMap k p = M.update
                    (\_ -> Just p)
                    k
    neighbors x   = getNeighbors neighborDistance x tree


-- | Assign random labels to the leaves of a tree in a clumped fashion
assignRandomClumpedProperties :: (Ord a, Eq b)
                              => [b]
                              -> Int
                              -> Tree (SuperNode a)
                              -> StdGen
                              -> PropertyMap a b
                              -> PropertyMap a b
assignRandomClumpedProperties propertyList neighborDistance tree g propertyMap =
    foldl' ( \acc (x, y)
          -> clumpIt neighborDistance tree x y acc)
    propertyMap
  . zip shuffledLeaves
  $ propertyList
  where
    shuffledLeaves = shuffle' (M.keys propertyMap) (M.size propertyMap) g

-- | Assign random labels to the leaves of a tree
assignRandomProperties :: (Eq a, Ord a)
                       => [b]
                       -> StdGen
                       -> PropertyMap a b
                       -> PropertyMap a b
assignRandomProperties propertyList g propertyMap = M.fromList
                                                  . zip shuffledLeaves
                                                  $ propertyList
  where
    shuffledLeaves = shuffle' (M.keys propertyMap) (M.size propertyMap) g

-- | Return the propertyMap
getPropertyMap :: (Ord a) => [a] -> PropertyMap a a
getPropertyMap x = M.fromList . zip x $ x
