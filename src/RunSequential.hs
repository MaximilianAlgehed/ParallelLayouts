{-# LANGUAGE GADTs #-}
import Test.QuickCheck
import ParallelLayout

runSequential :: Pll a b -> ([a] -> [b])
runSequential ((Pure f), _)              (a:_) = [f a] 
runSequential (Pad, _)                   (a:_) = [a]
runSequential ((Above l@(_, (i, _)) l'), _)   as = (runSequential l (take i as)) ++ (runSequential l' (drop i as))
runSequential ((Besides l@(_, (_, o)) l'@(_, (i, _))), _) as = (runSequential l') $ concat $ replicate (i `div` o) $ (runSequential l) as 
runSequential ((Overlay l@(_, (i, _)) l'@(_, (i', _))), _) as = zip (runSequential l (take i as)) (runSequential l' (take i' as))
runSequential ((Collapse x@(_, (ixs, oxs)) y@(_, (iys, oys))), _) as = o
        where
            o = runSequential y iy
            ox = runSequential x as
            iy = fitInto ox iys
            fitInto [] n = replicate n []
            fitInto xs n = zipWith (:) (take n xs) (fitInto (drop n xs) n)

-- | Check that prefix meets the specification
prop_prefix_scanl xs = length xs > 0 ==> tail (scanl (+) 0 xs) == runSequential (prefix_fan (length xs) (uncurry (+))) xs
