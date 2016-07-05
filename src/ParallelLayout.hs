-- | This implementation of patterns for parallel programs
-- | is a work in progress. It currently targets the Par monad
-- | and is focused on expressability. Performance is lacking...
-- | (i.e. it is the slowest thing on the face of the planet)
{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (

    Pll,

    (>->),

    (-=-),

    (>/>),

    (*>*)

)where
import Prelude hiding ((<*), (*>))
import Test.QuickCheck
import Data.GraphViz hiding (Arrow)
import TraceInternal
import Control.DeepSeq (NFData)
import Debug.Trace

-- Funky type for parallel computations
type Pll a b = ([IVar a] -> [IVar b] -> Par (), (Int, Int))

-- Shorthand for just one action
act f = (\[i] [o] -> fmap f (get i) >>= put o, (1,1))

-- | Compose two operations in parallel
above (u, (inu, ou)) (l, (inl, ol)) =
    ((\is os -> do
        fork $ u (take inu is) (take ou os)
        fork $ l (drop inu is) (drop ou os)), (inu+inl, ou+ol)) 

-- | Compose several operations in parallel
aboveL :: [Pll a b] -> Pll a b
aboveL = foldl1 above

-- | Compose two operations in sequence
besides (x, (ix,ox)) (y, (iy, oy)) =
    (\i o -> do
        -- | The intermidate results
        interms <- sequence $ replicate ox new 
        -- | Fork the second action with n/m repetitions of the
        -- | intermidiate results
        fork $ y (concat (replicate (iy `div` ox) interms)) o
        -- | Run the first computation
        x i interms, (ix, oy))

-- | Compose several operations in sequence
besidesL :: [Pll a a] -> Pll a a
besidesL = foldl1 besides

-- | Collapse parallel operations
collapse :: (NFData b) => Pll a b -> Pll [b] c -> Pll a c
collapse (x, (ixs, oxs)) (y, (iys, oys)) =
    ((\i o -> do
        ox <- sequence $ replicate oxs new
        iy <- sequence $ replicate iys new
        fork $ y iy o
        x i ox
        fitInto (replicate iys []) ox iy), (ixs, oys))
    where
        fitInto xs [] iy = sequence_ (zipWith put iy xs)
        fitInto xs ox iy =
            do ox' <- mapM get (take (length iy) ox)
               fitInto (zipWith (\x y -> y++[x]) ox' xs) (drop (length iy) ox) iy  

-- | Overlay one operation on another
overlay :: (NFData b, NFData c) => Pll a b -> Pll a c -> Pll a (b, c)
overlay (x, (ix, ox)) (y, (iy, oy)) =
    ((\is os -> do
                 oxs <- sequence $ replicate ox new
                 oys <- sequence $ replicate oy new
                 fork $ x (take ix is) oxs
                 fork $ y (take iy is) oys
                 intermx <- sequence $ map get oxs
                 intermy <- sequence $ map get oys
                 sequence_ $ zipWith put os $ zip intermx intermy), (max ix iy, min ox oy))

-- | Compose two operations in parallel
(-=-) = above

-- | Compose two operations in sequence
(>->) = besides

-- | Collapse operations in to another operation
(>/>) = collapse

-- | Overlay one operation on another
(-&-) :: (NFData b, NFData c) => Pll a b -> Pll a c -> Pll a (b, c)
(-&-) = overlay

-- Run a computation in the par monad, and only return the values produced in the end
runInPar :: (NFData a) => Pll a b -> [a] -> [b]
runInPar (pll, (ins, outs)) inputs = fst $ runPar $ do
                                        is <- sequence $ replicate ins new
                                        os <- sequence $ replicate outs new
                                        sequence_ (zipWith put is inputs)
                                        fork $ pll is os
                                        sequence $ map get os

-- An example
-- The structure represented is
--               +-(+1)-+ 
-- input -> (+1)-+-(*3)-+-sum -> output
--               +-(/3)-+
example :: Pll Double Double
example = act (+1) >-> aboveL (map act [(+1), (*3), (/3)]) >/> act sum

-- | Connect things in a parallel prefix manner.
-- | If p and p' both compute parallel prefix of their input, then p>*>p' computes
-- | parallel prefix of it's input
--
-- The function is very slow as it does not actually perform the operations in parallel,
-- rather it performs the left operation first and then the right one...
p@(_, (_, o1)) *>* p'@(_, (i2, _)) =
    (p -=- (aboveL (replicate (i2-1) (act id))))
    >->
    (aboveL (replicate (o1-1) (act id)) -=- p')

-- | The fan pattern
fan :: (NFData a) => Pll (a,a) a -> Pll a a -> Pll a a
fan fanout p@(_, (_, op)) =
    (
        act id >-> aboveL (replicate (op+1) (act id))
        -&-
        ((act id) -=- p)
    ) >-> (act fst -=- fanout)

-- | Primitive prefix sum
prefix :: (NFData a) => Int -> ((a,a)->a) -> Pll a a
prefix 1 _ = act id
prefix n op = fan (aboveL (replicate (n-1) (act op))) (prefix (n-1) op)

-- | Check that prefix meets the specification
prop_prefix_scanl xs = length xs > 0 ==> tail (scanl (+) 0 xs) == runInPar (prefix (length xs) (uncurry (+))) xs
