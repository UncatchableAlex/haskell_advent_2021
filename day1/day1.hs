import System.IO
main = do
 handle <- openFile "day1input.txt" ReadMode
 fileContents <- hGetContents handle
 let nums = map (read :: String -> Int) (words fileContents)
 (putStrLn.show.part1) nums
 (putStrLn.show.part2) nums

part1 :: [Int] -> Int
part1 (first:second:rest)
   | first < second = 1 + (part1 (second:rest))
   | otherwise = part1 (second:rest)
part1 [_] = 0

part2 :: [Int] -> Int
part2 ls = part1 $ map sum (groupBySizeN 3 ls)

groupBySizeN :: Int -> [a] -> [[a]]
groupBySizeN n list = res
 where
  firstN = take n list
  rest = drop 1 list
  nonBaseCase = groupBySizeN n rest
  res = if (length rest) < n then [firstN] else firstN:nonBaseCase