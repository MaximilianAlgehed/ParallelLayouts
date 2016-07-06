{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (

    Pll,

    Layout(..),

    act,

    (>->),

    (-=-),

    (>/>),

    (*>*),

    fan,

    example,

    prefix_fan

) where
import Prelude hiding ((<*), (*>))
import Data.GraphViz hiding (Arrow)
import Control.DeepSeq (NFData)
import Debug.Trace

data Layout a b where
    Pure     :: (a -> b) -> Layout a b
    Pad      :: Layout a a
    Above    :: Pll a b -> Pll a b -> Layout a b
    Besides  :: Pll a b -> Pll b c -> Layout a c
    Overlay  :: Pll a b -> Pll a c -> Layout a (b, c)
    Collapse :: Pll a b -> Pll [b] c -> Layout a c

paren s = "("++s++")"

instance Show (Layout a b) where
    show (Pure _)        = "*"
    show (Above l l')    = paren $ (show (fst l)) ++ " above " ++ (show (fst l'))
    show (Besides l l')  = paren $ (show (fst l)) ++ " besides " ++ (show (fst l'))
    show (Overlay l l')  = paren $ (show (fst l)) ++ " overlay " ++ (show (fst l'))
    show (Collapse l l') = paren $ (show (fst l)) ++ " collapse " ++ (show (fst l'))
    show Pad             = "-"

-- Funky type for parallel computations
type Pll a b = (Layout a b, (Int, Int))

act f = (Pure f, (1,1))

acts = map act

pad = (Pad, (1,1))

pads :: Int -> Pll a a
pads n = aboveL (replicate n pad)

-- | Compose two operations in parallel
above u@(_, (inu, ou)) l@(_, (inl, ol)) = (Above u l, (inu+inl, ou+ol)) 

-- | Compose several operations in parallel
aboveL :: [Pll a b] -> Pll a b
aboveL = foldl1 above

-- | Compose two operations in sequence
besides x@(_, (ix,ox)) y@(_, (iy, oy)) = (Besides x y, (ix, oy))

-- | Compose several operations in sequence
besidesL :: [Pll a a] -> Pll a a
besidesL = foldl1 besides

-- | Collapse parallel operations
collapse :: Pll a b -> Pll [b] c -> Pll a c
collapse x@(_, (ixs, oxs)) y@(_, (iys, oys)) = (Collapse x y, (ixs, oys))

-- | Overlay one operation on another
overlay :: Pll a b -> Pll a c -> Pll a (b, c)
overlay x@(_, (ix, ox)) y@(_, (iy, oy)) = (Overlay x y, (max ix iy, min ox oy))

-- | Compose two operations in parallel
(-=-) = above

-- | Compose two operations in sequence
(>->) = besides

-- | Collapse operations in to another operation
(>/>) = collapse

-- | Overlay one operation on another
(-&-) :: Pll a b -> Pll a c -> Pll a (b, c)
(-&-) = overlay

-- An example
-- The structure represented is
--               +-(+1)-+ 
-- input -> (+1)-+-(*3)-+-sum -> output
--               +-(/3)-+
example :: Pll Double Double
example = act (+1) >-> aboveL (acts [(+1), (*3), (/3)]) >/> act sum

-- | Connect things in a parallel prefix manner.
-- | If p and p' both compute parallel prefix of their input, then p>*>p' computes
-- | parallel prefix of it's input
--
-- The function is very slow as it does not actually perform the operations in parallel,
-- rather it performs the left operation first and then the right one...
p@(_, (_, o1)) *>* p'@(_, (i2, _)) =
    (p -=- pads (i2-1))
    >->
    (pads (o1-1) -=- p')

-- | The fan pattern
fan :: Pll (a,a) a -> Pll a a -> Pll a a
fan fanout p@(_, (_, op)) =
    (
        (pad >-> pads (op+1))
        -&-
        (pad -=- p)
    ) >-> (act fst -=- fanout)

-- | Primitive prefix sum
prefix_fan :: Int -> ((a,a)->a) -> Pll a a
prefix_fan 1 _  = pad 
prefix_fan n op = fan (aboveL (replicate (n-1) (act op))) (prefix_fan (n-1) op)
