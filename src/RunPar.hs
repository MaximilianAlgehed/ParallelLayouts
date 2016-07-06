{-# LANGUAGE GADTs #-}
import Control.DeepSeq
import TraceInternal
import ParallelLayout
-- Funky type for parallel computations
type PllPar a b = [IVar a] -> [IVar b] -> Par ()

convert :: (NFData a, NFData b) => Pll a b -> PllPar a b
convert (Pad, _) = \[i] [o] -> get i >>= put o

-- Shorthand for just one action
convert (Pure f, _) = \[i] [o] -> fmap f (get i) >>= put o

-- | Compose two operations in parallel
convert (Below u@(_, (inu, ou)) l@(_, (inl, ol)), _) = \is os -> do
        fork $ (convert u) (take inu is) (take ou os)
        fork $ (convert l) (drop inu is) (drop ou os)

-- | Compose two operations in sequence
convert (Besides x@(_, (ix,ox)) y@(_, (iy, oy)), _) = \i o -> do
        -- | The intermidate results
        interms <- sequence $ replicate ox new 
        -- | Fork the second action with n/m repetitions of the
        -- | intermidiate results
        fork $ (convert y) (concat (replicate (iy `div` ox) interms)) o
        -- | Run the first computation
        (convert x) i interms

-- | Collapse parallel operations
convert (Collapse x@(_, (ixs, oxs)) y@(_, (iys, oys)),_) = \i o -> do
        ox <- sequence $ replicate oxs new
        iy <- sequence $ replicate iys new
        fork $ (convert y) iy o
        (convert x) i ox
        fitInto (replicate iys []) ox iy
    where
        fitInto xs [] iy = sequence_ (zipWith put iy xs)
        fitInto xs ox iy =
            do ox' <- mapM get (take (length iy) ox)
               fitInto (zipWith (\x y -> y++[x]) ox' xs) (drop (length iy) ox) iy  

-- | Overlay one operation on another
convert (Overlay x@(_, (ix, ox)) y@(_, (iy, oy)),_) = \is os -> do
                 oxs <- sequence $ replicate ox new
                 oys <- sequence $ replicate oy new
                 fork $ (convert x) (take ix is) oxs
                 fork $ (convert y) (take iy is) oys
                 intermx <- sequence $ map get oxs
                 intermy <- sequence $ map get oys
                 sequence_ $ zipWith put os $ zip intermx intermy

-- Run a computation in the par monad, and only return the values produced in the end
runInPar :: (NFData a, NFData b) => Pll a b -> [a] -> [b]
runInPar pll@(_, (ins, outs)) inputs = fst $ runPar $ do
                                        is <- sequence $ replicate ins new
                                        os <- sequence $ replicate outs new
                                        sequence_ (zipWith put is inputs)
                                        fork $ (convert pll) is os
                                        sequence $ map get os
