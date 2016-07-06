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

-- Run a computation in the par monad, and only return the values produced in the end
runInPar :: (NFData a) => Pll a b -> [a] -> [b]
runInPar (pll, (ins, outs)) inputs = fst $ runPar $ do
                                        is <- sequence $ replicate ins new
                                        os <- sequence $ replicate outs new
                                        sequence_ (zipWith put is inputs)
                                        fork $ pll is os
                                        sequence $ map get os
