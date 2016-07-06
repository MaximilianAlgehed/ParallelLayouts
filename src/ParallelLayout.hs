{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (
    Pll,
    Layout(..),
    act,
    (->-),
    (-=-),
    (-/-),
    (*>*),
    (-!-),
    belowL,
    belowN,
    pads,
    evens,
    odds,
    liftListOp,
    fan,
    riffle,
    unriffle,
    interleave,
    example,
    prefix_fan,
    sklansky
) where
import Data.List

-- | A type for layouts of circuits (parallel computations)
data Layout a b where
    Pure     :: (a -> b) -> Layout a b
    Pad      :: Layout a a
    Below    :: Pll a b -> Pll a b -> Layout a b
    Besides  :: Pll a b -> Pll b c -> Layout a c
    Overlay  :: Pll a b -> Pll a c -> Layout a (b, c)
    Collapse :: Pll a b -> Pll [b] c -> Layout a c

paren s = "("++s++")"

instance Show (Layout a b) where
    show (Pure _)        = "*"
    show (Below l l')    = paren $ (show (fst l)) ++ " below " ++ (show (fst l'))
    show (Besides l l')  = paren $ (show (fst l)) ++ " besides " ++ (show (fst l'))
    show (Overlay l l')  = paren $ (show (fst l)) ++ " overlay " ++ (show (fst l'))
    show (Collapse l l') = paren $ (show (fst l)) ++ " collapse " ++ (show (fst l'))
    show Pad             = "-"

-- Funky type for parallel computations
type Pll a b = (Layout a b, (Int, Int))

-- Pure action
act f = (Pure f, (1,1))

-- Many pure actions
acts = map act

-- Padding
pad = (Pad, (1,1))

-- Pad many times
pads :: Int -> Pll a a
pads n = belowN n pad

-- | Compose two operations in parallel
below u@(_, (inu, ou)) l@(_, (inl, ol)) = (Below u l, (inu+inl, ou+ol)) 

-- | Compose several operations in parallel
belowL :: [Pll a b] -> Pll a b
belowL = foldl1 below

-- | Many belows...
belowN n op = belowL (replicate n op)

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
(-=-) = below

-- | Compose two operations in sequence
(->-) = besides

-- | Collapse operations in to another operation
(-/-) = collapse

-- | Overlay one operation on another
(-&-) :: Pll a b -> Pll a c -> Pll a (b, c)
(-&-) = overlay

-- | Select item n from m
select :: Int -> Int -> Pll a a
select n m = pads m -/- (act (!!n))

-- Selection shorthand
(-!-) = select

-- | Apply op to even elements
evens :: Int -> Pll (a,a) a -> Pll a a
evens n op = belowN (n `div` 2) $ (pad ->- pads 2) -&- (pads 2) ->- (act fst -=- op)

-- | Apply op to odd elements
odds :: Int -> Pll (a,a) a -> Pll a a
odds n op = pad -=- evens (n-2) op -=- pad

-- An example
-- The structure represented is
--               +-(+1)-+ 
-- input -> (+1)-+-(*3)-+-sum -> output
--               +-(/3)-+
example :: Pll Double Double
example = act (+1) ->- belowL (acts [(+1), (*3), (/3)]) -/- act sum

-- | Connect things in a parallel prefix manner.
-- | If p and p' both compute parallel prefix of their input, then p>*>p' computes
-- | parallel prefix of it's input
p@(_, (_, o1)) *>* p'@(_, (i2, _)) =
    (p -=- pads (i2-1))
    ->-
    (pads (o1-1) -=- p')

-- | The fan pattern
fan :: Pll (a,a) a -> Pll a a -> Pll a a
fan fanout p@(_, (_, op)) =
    (
        (pad ->- pads (op+1))
        -&-
        (pad -=- p)
    ) ->- (act fst -=- fanout)

-- | Allows list operations to be performed on a layout
liftListOp :: Int -> ([a] -> [a]) -> Pll a a
liftListOp n f = pads n -/- act f ->- belowL [act (!!i) | i <- [0..(n-1)]]

-- | Riffle a list
riffle :: Int -> Pll a a
riffle n = liftListOp n riffleList
    where
        riffleList lst = helper (take ((length lst) `div` 2) lst) (drop ((length lst) `div` 2) lst)
        helper [] xs = xs
        helper xs [] = xs
        helper (x:xs) (y:ys) = x:y:helper xs ys

-- | Unriffle a list
unriffle :: Int -> Pll a a
unriffle n = liftListOp n unriffleList
    where
        unriffleList xs = [x | (x, i) <- zip xs [0..], even i] ++ [x | (x,i) <- zip xs [0..], odd i]

-- | Interleave an operation
interleave :: Pll a b -> Pll a b
interleave p@(_, (i,o)) = unriffle (2*i) ->- (p -=- p) ->- riffle (2*o)

-- | Primitive prefix sum
prefix_fan :: Int -> ((a,a)->a) -> Pll a a
prefix_fan 1 _  = pad 
prefix_fan n op = fan (belowN (n-1) (act op)) (prefix_fan (n-1) op)

-- | sklansky parallel prefix network
sklansky :: ((a,a) -> a) -> Int -> Pll a a
sklansky op 1 = pad
sklansky op 2 = fan (act op) pad
sklansky op n = sklansky op ls *>* fan (belowN rs (act op)) (sklansky op rs) 
    where
        ls = n `div` 2 + 1
        -- fan pads with 1 on the left, so we need to reduce the size of
        -- the right hand argument
        rs = if even n then
                n `div` 2 - 1
             else
                n `div` 2
