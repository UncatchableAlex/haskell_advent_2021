import System.IO

main :: IO ()
main = do
 handle <- openFile "input.txt" ReadMode
 fileContents <- hGetContents handle
 let lns = map words (lines fileContents)
 (print.part1) lns
 (print.part2) lns


part1 :: [[String]] -> Int
part1 lns = sum' forwards * (sum' downs - sum' ups)
 where
  parseInstr instr = (head instr, read (last instr) :: Int)
  instrs = map parseInstr lns
  forwards = filter (\instr -> fst instr == "forward") instrs
  downs = filter (\instr -> fst instr == "down") instrs
  ups = filter (\instr -> fst instr == "up") instrs
  sum' ls = sum (map snd ls)


part2 :: [[String]] -> Int
part2 lns = finalResult (foldl1 combineInstrs instrs)
 where
  -- (direction, magnitude, depth, aim, horiz)
  parseInstr instr = (head instr, read (last instr) :: Int, 0, 0, 0)
  -- add a null instruction to the front
  instrs = ("na", 0, 0, 0, 0) : map parseInstr lns
  combineInstrs (_,_,dep,aim,horiz) (dir,mag,_,_,_) =
   case dir of
    "down" -> ("", 0, dep, aim+mag, horiz)
    "up" -> ("", 0, dep, aim-mag, horiz)
    "forward" -> ("", 0, dep+(aim*mag), aim, horiz+mag)
  finalResult (_,_,dep,_,horiz) = horiz * dep