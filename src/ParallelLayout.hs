{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (

    Pll,

    (<|), (|>),

    (<*), (*>),

    (</), (/>)

)where
import Prelude hiding ((<*), (*>))
import Data.GraphViz hiding (Arrow)
import TraceInternal
import Control.DeepSeq (NFData)
import Debug.Trace

-- Funky type for parallel computations
type Pll a b = [IVar a] -> [IVar b] -> Par ()

-- | Execute a list of pure actions
acts [] _ _ = return ()
acts (f:fs) (i:is) (o:os) =
    do
        act f [i] [o] 
        acts fs is os

-- Shorthand for just one action
act f [i] [o] = fmap f (get i) >>= put o 

-- | Compose two operations in parallel
above (len, len') u l is os =
    do
        fork $ u (take len is) (take len' os)
        fork $ l (drop len is) (drop len' os) 

-- | Compose several operations in parallel
aboveL = combine above

-- | General listed composition of operations
combine f xs ys = foldr ($) (last ys) $ zipWith f xs (init ys)

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

-- | Compose several operations in sequence
besidesL = combine besides

-- | Collapse parallel operations
collapse :: (NFData b) => (Int, Int) -> Pll a b -> Pll [b] c -> Pll a c
collapse (m,n) x y i o =
    do
        ox <- sequence $ replicate m new
        iy <- sequence $ replicate n new
        fork $ y iy o
        x i ox
        fitInto (replicate n []) ox iy
    where
        fitInto xs [] iy = sequence_ (zipWith put iy xs)
        fitInto xs ox iy =
            do ox' <- mapM get (take (length iy) ox)
               fitInto (zipWith (\x y -> y++[x]) ox' xs) (drop (length iy) ox) iy  

-- | Compose two operations in parallel
(<|) x d = \y i o -> above d x y i o
(|>) = ($)
infixr 0 |>

-- | Compose two operations in sequence
(<*) x d = \y i o -> besides d x y i o
(*>) = ($)
infixr 0 *>

-- | Collapse parallel operations into fewer parallel operations
(</) x d = \y i o -> collapse d x y i o
(/>) = ($)
infixr 0 />

-- Run a computation in the par monad, and only return the values produced in the end
runInPar :: (NFData a) => Int -> Int -> Pll a b -> [a] -> [b]
runInPar ins outs pll inputs = fst $ runPar $ do
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
example = act (+1) <*(1,3)*>
          aboveL [(1,1), (1,1)] (map act [(+1), (*3), (/3)])
          </(3,1)/> act sum

-- Constructing parallel prefix networks
(>*>) :: (NFData a) => Pll a a -> Pll a a -> Pll a a
(p1 >*> p2) is os = (p1' <*(n,n)*> p2') is os
    where
        p1' = (p1 <|(m,m)|> aboveL (replicate (n-m-1) (1,1)) (replicate (n-m) (act id)))
        p2' = (aboveL (replicate (m-2) (1,1)) (replicate (m-1) (act id)) <|(m-1,m-1)|> p2)
        n   = length is
        m   = (n `div` 2) + 1

test :: Pll Int Int
test = (aboveL [(1,1), (1,1)] [act (+1), act (+1)]) >*> (aboveL [(1,1), (1,1)] [act (+1), act (+1)])
