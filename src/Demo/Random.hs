module Demo.Random ( randomInput ) where

import qualified Data.Map as M
import qualified System.Random as R
import Demo.Types
import qualified Demo.Links as Links
import qualified Data.List as L


-- Config!
randomEmptyCells :: Int -> Int
randomEmptyCells numCells = numCells `div` 10
randomValueRange = ('A', 'Z')

infiniR :: R.RandomGen g => g -> [Char]
infiniR = R.randomRs randomValueRange

shuffle :: R.RandomGen g => [a] -> g -> [a]
shuffle as g = 
  let ln = (L.length as) - 1
      m = (M.fromList . zip [0..]) as
      rs = R.randomRs (0, ln) g
      f _ (m,(a:b:rs)) = ((swap a b m), rs)
  in (snd . unzip . M.toList . fst) (foldr f (m,rs) [0..ln])

swap :: Ord a => a -> a -> M.Map a b -> M.Map a b
swap a b m = 
  let av = (M.lookup a m)
      bv = (M.lookup b m)
  in case (av,bv) of
       (Just avv, Just bvv) -> (M.insert a bvv . M.insert b avv) m
       _ -> m

type Pair = (Int, Int)

randomInput :: Int -> Int -> IO InputState
randomInput start size = do g <- R.newStdGen
                            let ps = findPairs start size g
                                sps = shuffle ps g
                                (h,m) = mkMem start size (infiniR g) sps
                            print ("RRRANDO: " ++ (show sps))
                            print ("MMMEM: " ++ (show m))
                            return (InSt start size h m)

findPairs :: R.RandomGen g => Int -> Int -> g -> [Pair]
findPairs i s g = r i s (randomEmptyCells s) g
  where r index size numEmpties gen = 
          if numEmpties == 0
             then mkPairs [index..(index + size - 1)] 
             else let high = (size - 1 - (numEmpties - 1)) `div` 2
                      (a,nextGen) = R.randomR (0,high) gen
                      thisEmpty = index + (a * 2)
                      safeList = [index..(thisEmpty - 1)]
                      nextIndex = thisEmpty + 1
                      nextSize = size - (nextIndex - index)
                  in (mkPairs safeList) ++ (r nextIndex
                                              nextSize
                                              (numEmpties - 1)
                                              nextGen) 
        
mkPairs :: [a] -> [(a,a)]
mkPairs (a:b:cs) = (a,b) : mkPairs cs
mkPairs _ = []

endAddressString = "/"

mkMem :: Int -> Int -> [Char] -> [Pair] -> (String, MemSt)
mkMem start size rvals pairs = 
  let h = (show . fst . head) pairs
      r (thisPair : nextPair : ps) (val : vs) = 
        let box = fst thisPair
            arr = snd thisPair
            addr = show (fst nextPair)
        in (box, val) : (arr, addr) : (r (nextPair:ps) vs)
      r (endPair : []) (val : _) = 
        let box = fst endPair
            arr = snd endPair
            addr = endAddressString
        in (box, val) : (arr, addr) : []
  in (h, mkMemSt (r pairs (fmap (: []) rvals)))
