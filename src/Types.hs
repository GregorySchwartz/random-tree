-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

-- Built-in
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Trans.Maybe

--Cabal
import Control.Monad.Random

data TreeState a = TreeState { size :: Int }

data TreeConfig = TreeConfig { maxSize     :: Int
                             , minSize     :: Int
                             , minChildren :: Int
                             , maxChildren :: Int }

newtype ReaderStateRandom a = ReaderStateRandom
    { runReaderStateRandom :: ReaderT TreeConfig (StateT (TreeState Int) (MaybeT (Rand StdGen))) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadPlus
             , MonadRandom
             , MonadState (TreeState Int)
             , MonadReader TreeConfig )

-- Basic
type Label = String

-- Advanced
type DistanceMap a = M.Map a [(a, Int)]
type LabelList a   = [a]
type LabelMap a    = M.Map a a
