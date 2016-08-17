import ParallelLayout
import RunSequential

-- | Simple dot product
dot :: Num a => Pll x a -> Pll x a -> Pll x a
dot xs ys = (xs -&- ys) ->- multiply -/- act sum
    where
        multiply = belowN (max (outs xs) (outs ys)) $ act $ uncurry (*)

-- | Generalized fold with granularity control
foldl_ :: Int -> (a -> a -> a) -> a -> Pll x a -> Pll x a
foldl_ 0 f e xs = xs -/- (act $ foldl f e)
foldl_ n f e xs = xs'
    where
        xs'     = xs ->- (folderU -=- folderL) -/- act (\[a, b] -> f a b)
        folderU = foldl_ (n-1) f e $ pads $ outs xs `div` 2
        folderL = foldl_ (n-1) f e $ pads $ if even (outs xs) then outs xs `div` 2 else (outs xs `div` 2) + 1

-- | Multiply a matrix with a vector
mVecMult :: (Num a) => [[a]] -> Pll [a] [a]
mVecMult xs = pad ->- (belowL . acts) [sum . (zipWith (*) x) | x <- xs] -/- pad

-- | A layer in a neural network
nnLayer :: (Num a) => (a -> a) -> [[a]] -> Pll [a] [a]
nnLayer f xs = mVecMult xs ->- act (map f)
