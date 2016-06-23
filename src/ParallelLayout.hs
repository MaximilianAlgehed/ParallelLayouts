{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (
    Pll,
    (<|>),
    (<>),
    (><)
)where
import Data.GraphViz hiding (Arrow)
import TraceInternal
import Control.DeepSeq (NFData)

-- The type of a parallel computation
-- this type is a placeholder for a later
-- type which will likely look more like:
-- (Int, Int) -> Chan a -> Chan b -> SomethingToPutOnParallela
-- and use some kind of "forkOn" function rather
-- than just fork
type Pll a b = IVar a -> IVar b -> Par ()

-- | Execute a pure action
arr f i o = fmap f (get i) >>= put o

-- | Compose two operations in parallel
above u l i o =
    do
        intermA <- new
        intermB <- new
        fork $ u i intermA
        fork $ l i intermB
        a <- get intermA
        b <- get intermB
        put o (a, b)

-- | Compose two operations in sequence
besides x y i o =
    do
        interm <- new
        fork $ y interm o
        x i interm

-- | Compose two operations in parallel
-- | in sequence on the same core
ontop x y i o =
    do
        interm <- new
        x i interm
        y interm o

-- | Compose two operations in parallel
(<|>) :: (NFData b, NFData c) => Pll a b -> Pll a c -> Pll a (b, c)
(<|>) = above

-- | Compose two operations in sequence
(<>) :: (NFData b, NFData c) => Pll a b -> Pll b c -> Pll a c
(<>) = besides

-- | Compose two operations in parallel
-- | in sequence on the same core
(><) :: (NFData b, NFData c) => Pll a b -> Pll b c -> Pll a c
(><) = ontop
