module Demo.Links ( randomList, getAtIndex, readInt, compute ) where

import Text.Read (readMaybe)


compute :: [String] -> [String]
compute ss = 
  let 
    r i vals list seen = 
      case readInt =<< (getAtIndex (i) vals) of
        Just x -> if x `elem` seen then list else s x vals list (x:seen)
        _      -> list

    s i vals list seen =
      case getAtIndex i vals of
        Just y -> y : (r (i+1) vals list seen)
        _      -> "" : (r (i+1) vals list seen)

  in s 0 ss [] []

readInt :: String -> Maybe Int
readInt = readMaybe

getAtIndex :: Int -> [a] -> Maybe a
getAtIndex 0 (x:_) = Just x
getAtIndex i (_:xs) = getAtIndex (i-1) xs
getAtIndex _ _ = Nothing

randomList :: IO ([String])
randomList = return ["a", "3", "", "b", "8", "d", "14", "", "c", "5"]
