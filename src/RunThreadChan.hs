module RunThreadChan where
import SimpleExamples
import Control.Concurrent.Chan

-- TO DO:
-- Take in to account "On" mapping, maybe by
-- using a type like 
-- [Chan a] -> ([ThreadID] -> IO (), [Chan b])
-- or something like that
type IR a b = [Chan a] -> (IO (), [Chan b])

runThreadChan :: Pll a b -> [a] -> IO [b]
runThreadChan p as = do
                        is <- sequence $ replicate (ins p) newChan
                        sequence_ $ zipWith putChan is as
                        let ir = runIR p is
                        forkIO $ fst ir
                        sequence $ zipWith getChan (snd is)

runIR :: Pll a b -> IR a b
runIR (Pad, _) (i:_) = (return (), [i]) -- Zero overhead ID, or something...
runIR _         _    = undefined        -- To do in Lund
