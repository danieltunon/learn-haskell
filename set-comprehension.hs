module SetComprehension where
-- import Safe (tailSafe)

import Data.Map.fromListWith
import Data.Map.toList

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n],
                        y <- [1..n],
                        z <- [1..n],
                        x ^ 2 + y ^ 2 == z ^ 2,
                        x == n || y == n || z == n ]

-- a number is perfect if it is equal to the sum of all of it's factors
-- excluding the number itself
-- return a list of perfects up to a give limit
factors :: (Num a, Enum a, Integral a) => a -> [a]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [2..n], x == (sum . init $ factors x) ]

strs :: [String]
strs = ["boy", "girl", "dog", "cactus", "entropy"]
dotProduct :: [String] -> [String]
-- dotProduct css | null css = [""]
--                | otherwise = [ c : cs | cs <- dotProduct(tail css), c <- head css]
dotProduct [] = [""]
dotProduct (cs:css) = [ c:s | s <- dotProduct css, c <- cs ]

-- dPapi :: [String] -> String
-- patternMatch :: String -> String -> Bool
--
type PatternCount = (Char, Int)
patternCounter :: String -> [PatternCount]
patternCounter ps = toList $ fromListWith (+) [ (p, 1) | p <- ps ]

minOtherChars pcs n = subtract n $ sum [snd pc | pc <- pcs]

mapMaxLen pcs t = [(p, (t - minOtherChars pcs c) `div` c) | (p, c) <- pcs]

lenCombos :: [(Char, Int)] -> [[(Char, Int)]]
lenCombos ((p, m):[]) = [[(p, n)] | n <- [1..m]]
lenCombos ((p, m):pms) = [ (p, n):pls | pls <- lenCombos pms, n <- [1..m] ]

isRightLen l pcs lcs = l == sum (map snd $ toList $ fromListWith (*) $ pcs ++ lcs)

combos p s = filter (isRightLen s p) (lenCombos $ mapMaxLen p s)

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (b:bs) = b && allTrue bs

merge :: Ord a => [a] -> [a] -> [a]
merge [a] [] = [a]
merge [] [a] = [a]
merge (x:xs) (y:ys) | null (x:xs) = y:ys
                    | null (y:ys) = x:xs
                    | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
