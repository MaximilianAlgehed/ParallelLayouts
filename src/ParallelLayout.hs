{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module ParallelLayout (
    Pll,
    Layout(..),
    Source,
    act,
    acts,
    ins,
    outs,
    fork,
    frst,
    scnd,
    (->-),
    (-=-),
    (-/-),
    (*>*),
    (-!-),
    (-&-),
    belowL,
    belowN,
    besidesL,
    besidesN,
    pad,
    pads,
    evens,
    odds,
    onOdd,
    liftListOp,
    fan,
    riffle,
    unriffle,
    interleave,
    wrap,
    example,
    prefix_fan,
    sklansky,
    ladF,
    spreadSource
) where
import Data.List

-- | A type for layouts of circuits (parallel computations)
data Layout a b where
    Pure     :: (a -> b) -> Layout a b
    Fst      :: Layout (a, b) a
    Snd      :: Layout (a, b) b
    Fork     :: Layout a (a, a)
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

ins = fst . snd
outs = snd . snd

-- | A source of values in the form of a timeseries
type Source a = Pll Int a

frst = (Fst, (1,1))
scnd = (Snd, (1,1))
fork = (Fork, (1,1))

-- Pure action
act f = (Pure f, (1,1))

-- Many pure actions
acts = map act

-- Padding
pad = (Pad, (1,1))

-- Pad many times
pads :: Int -> Pll a a
pads 0 = error "pads: Can't pad with 0 elements"
pads n = belowN n pad

-- | Compose two operations in parallel
below u@(_, (inu, ou)) l@(_, (inl, ol)) = (Below u l, (inu+inl, ou+ol)) 

-- | Compose several operations in parallel
belowL :: [Pll a b] -> Pll a b
belowL = foldl1 below

-- | Many belows...
belowN 0 _  = error "belowN: Can't place 0 elements below each other"
belowN n op = belowL (replicate n op)

-- | Compose two operations in sequence
besides x@(_, (ix,ox)) y@(_, (iy, oy)) = (Besides x y, (ix, oy))

-- | Compose several operations in sequence
besidesL :: [Pll a a] -> Pll a a
besidesL = foldl1 besides

besidesN 0 _ = error "besidesN: Can't place 0 elements in sequence"
besidesN n op = besidesL (replicate n op)

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

-- Spread a source out
spreadSource :: Int -> Source a -> Source a
spreadSource n source = pad ->- belowL [act (i+) ->- source | i <- [0..(n-1)]]

-- | Apply op to even elements
evens :: Int -> Pll (a,a) a -> Pll a a
evens n op
    | n <= 0    = error "evens: n <= 0"
    | n == 1    = pad
    | otherwise = belowN (n `div` 2) $ (pad ->- pads 2) -&- (pads 2) ->- (frst -=- op)

-- | Apply op to odd elements
odds :: Int -> Pll (a,a) a -> Pll a a
odds 1 op       = pad
odds 2 op       = ((pad ->- pads 2) -&- (pads 2)) ->- (frst -=- op)
odds n op
    | n <= 0    = error "odds: n < 0"
    | otherwise = pad -=- evens (n-2) op -=- pad

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
p@(_, (_, 1)) *>* p'@(_, (1, _))    = p ->- p'
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
liftListOp n f
    | n <= 0 = error "Can't lift a list operation on empty lists"
    | otherwise = pads n -/- act f ->- belowL [act (!!i) | i <- [0..(n-1)]]

-- | Riffle a list
riffle :: Int -> Pll a a
riffle n
    | n <= 0    = error "Can't riffle less than or equal to 0 elements"
    | otherwise = liftListOp n riffleList
    where
        riffleList lst = helper (take ((length lst) `div` 2) lst) (drop ((length lst) `div` 2) lst)
        helper [] xs = xs
        helper xs [] = xs
        helper (x:xs) (y:ys) = x:y:helper xs ys

-- | Unriffle a list
unriffle :: Int -> Pll a a
unriffle n
    | n <= 0    = error "Can't unriffle less than or equal to 0 elements"
    | otherwise = liftListOp n unriffleList
    where
        unriffleList xs = [x | (x, i) <- zip xs [0..], even i] ++ [x | (x,i) <- zip xs [0..], odd i]

-- | Apply to the odd elements
onOdd p@(_, (i, o)) = unriffle (2*i) ->- ((pads i ->- pads o) -=- p) ->- riffle (2*o)

-- | wrapping
wrap op circ@(_, (i, o))
    | i <= 0    = error "wrap: can't wrap 0 inputs"
    | o <= 0    = error "wrap: can't wrap 0 outputs"
    | otherwise = evens (2*i) op ->- onOdd circ ->- odds (2*o) op

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

-- | Ladner Fischer networks
ladF :: ((a,a) -> a) -> Int -> Int -> Pll a a
ladF op _ 1 = pad
ladF op _ 2 = ((pad ->- pads 2) -&- pads 2) ->- (act fst -=- (act op))
ladF op 0 n
    | n <= 0    = error "ladF: Can't create a 0 width ladF network"
    | otherwise = ladF op 1 ls *>* fan'
        where
            -- fan out
            fan' = if rs <= 0 then
                        pad
                   else
                        fan (belowN rs (act op)) (ladF op 0 rs)
            ls = n `div` 2 + 1
            -- fan pads with 1 on the left, so we need to reduce the size of
            -- the right hand argument
            rs = if even n then
                    n `div` 2 - 1
                 else
                    n `div` 2
ladF op k n 
    | n <= 0    = error "ladF: Can't create a 0 width ladF network"
    | otherwise = wrap (act op) (ladF op (k-1) (n `div` 2))
