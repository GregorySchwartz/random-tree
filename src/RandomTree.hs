-- RandomTree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the creation of a random tree

module RandomTree where

-- Built-in
import Data.Tree
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative

--Cabal
import Control.Monad.Random
import Control.Lens ((??))

-- Local
import Types
import Tree
import Label

-- | Run the monad transformer for the generation of a random tree. The
-- minChildren value, if chosen, results in a leaf
runTree :: ReaderStateRandom a
        -> Int
        -> Int
        -> Int
        -> Int
        -> StdGen
        -> IO (Maybe a)
runTree start minS maxS minC maxC g = do
    let treeState = TreeState { size = 0 }
    let treeConfig = TreeConfig { maxSize     = maxS
                                , minSize     = minS
                                , minChildren = minC
                                , maxChildren = maxC }
    if minS > maxS
        then error "Minimum size is larger than maximum size."
        else return . (evalRand ?? g)
                    . runMaybeT
                    . (evalStateT ?? treeState)
                    . (runReaderT ?? treeConfig)
                    . runReaderStateRandom
                    $ start

-- | The recursion for each step of the tree
treeRecursion :: ReaderStateRandom (Tree Int)
treeRecursion = do
    treeConfig <- ask
    treeState <- get
    -- Cut it short if too big
    when (size treeState > maxSize treeConfig) mzero
    -- Cut it short if just right
    when ( size treeState >= minSize treeConfig
        && size treeState <= maxSize treeConfig )
        $ return ()
    -- Otherwise continue
    put (treeState { size = size treeState + 1 })
    r <- getRandomR (minChildren treeConfig, maxChildren treeConfig)
    if r == minChildren treeConfig
        then return Node { rootLabel = size treeState, subForest = [] }
        else do
            newSubForest <- replicateM r treeRecursion
            return Node { rootLabel = size treeState
                        , subForest = newSubForest }

-- | The check for the lower bound: if not fulfilled, returns Nothing
checkLowerBound :: ReaderStateRandom (Tree Int)
checkLowerBound = do
    put TreeState { size = 0 }
    tree <- treeRecursion
    treeState <- get
    treeConfig <- ask
    guard $ size treeState >= minSize treeConfig
    return tree

-- | Recursion which continues to make trees until the bounds are met
getTree :: ReaderStateRandom (Tree Int)
getTree = checkLowerBound `mplus` getTree

-- | Return trees to test this stuff on
makeTree :: [String] -> Int -> Int -> Int -> Int -> Int -> IO ()
makeTree labelList neighborDistance minS maxS minC maxC = do
    gen1         <- newStdGen
    gen2         <- newStdGen
    gen3         <- newStdGen

    (Just intTree) <- runTree getTree minS maxS minC maxC gen1

    let tree        = show <$> intTree
    putStr . drawTree $ tree

    let labelMap        = getLabelMap . leaves $ tree
        filledLabelList = take (length . leaves $ tree) . concat . repeat $ labelList
        distanceMap     = getDistanceMap tree
        newLabelMap     = assignRandomClumpedLabels
                          filledLabelList
                          neighborDistance
                          distanceMap
                          gen2
                          gen3
                          labelMap
        newUniformLabelMap = assignRandomLabels
                             filledLabelList
                             gen2
                             gen3
                             labelMap
        newTree         = relabelTree newLabelMap tree
    putStr . drawTree $ newTree
