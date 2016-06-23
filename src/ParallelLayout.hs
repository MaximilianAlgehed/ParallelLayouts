{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (
    Pll,
    (<|>),
    (<*),
    (*>)
)where
import Prelude hiding ((<*), (*>))
import Data.GraphViz hiding (Arrow)
import TraceInternal
import Control.DeepSeq (NFData)
import Debug.Trace

-- Funky type for parallel computations
type Pll a b = [IVar a] -> [IVar b] -> Par ()

-- | Execute a pure action
arr [] _ _ = return ()
arr (f:fs) (i:is) (o:os) =
    do
        fork $ fmap f (get i) >>= put o
        arr fs is os

-- | Compose two operations in parallel
above u l is os =
    do
        let len  = length is `div` 2
        let len' = length os `div` 2
        fork $ u (take len is) (take len' os)
        fork $ l (drop len is) (drop len' os) 

-- | Compose two operations in sequence
besides (m,n) x y i o =
    do
        -- | The intermidate results
        interms <- sequence $ replicate m new 
        -- | Fork the second action with n/m repetitions of the
        -- | intermidiate results
        fork $ y (concat (replicate (n `div` m) interms)) o
        -- | Run the first computation
        x i interms

-- | Collapse parallel operations
collapse (m,n) x y i o = undefined

-- | Compose two operations in parallel
(<|>) :: (NFData b) => Pll a b -> Pll a b -> Pll a b
(<|>) = above

-- | Compose two operations in sequence
(<*) x d = \y i o -> besides d x y i o
(*>) = ($)

-- | Collapse parallel operations into fewer parallel operations
(/*) x d = \y i o -> collapse d x y i o
(*\) = ($)
