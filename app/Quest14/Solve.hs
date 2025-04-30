{-# LANGUAGE TupleSections #-}
module Quest14.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import qualified Data.Foldable     as S
import           Data.List.Split   (splitOn)
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Text.Printf       (printf)

questPrefix = "app/Quest14/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type IJK   = (Int, Int, Int)
type Input = [[Direction]]

data Direction =
   U Int |
   D Int |
   R Int |
   L Int |
   F Int |
   B Int
   deriving Show

data Branch = Branch {
   end      :: IJK,
   segments :: S.Set IJK
}

getIJK :: Direction -> IJK
getIJK (R v) = ( v, 0,  0)
getIJK (L v) = (-v, 0,  0)
getIJK (U v) = ( 0,  v, 0)
getIJK (D v) = ( 0, -v, 0)
getIJK (F v) = ( 0, 0,  v)
getIJK (B v) = ( 0, 0, -v)

parseDirection :: String -> Direction
parseDirection direction = case d of
   'R' -> R v
   'L' -> L v
   'U' -> U v
   'D' -> D v
   'F' -> F v
   'B' -> B v
   where
      d = head direction
      v = read $ tail direction

parseInput :: FilePath -> IO Input
parseInput filePath = map (map parseDirection . splitOn ",") . lines <$> readFile filePath

addIJK :: IJK -> IJK -> IJK
addIJK (i1, j1, k1) (i2, j2, k2) = (i1 + i2, j1 + j2, k1 + k2)

solveBranches :: Input -> [Branch]
solveBranches = map ((\b -> Branch { end = fst b, segments = snd b } ) . (solveBranch S.empty . map getIJK))
   where
      solveBranch path = foldr traversePath (start, path) . reverse
         where
            start = (0, 0, 0)
            traversePath dir (cur@(curI, curJ, curK), path) = (to, path')
               where
                  to@(toI, toJ, toK) = addIJK cur dir
                  (minI, maxI) = (min curI toI, max curI toI)
                  (minJ, maxJ) = (min curJ toJ, max curJ toJ)
                  (minK, maxK) = (min curK toK, max curK toK)
                  path' = foldr S.insert path [(i, j, k) |
                                               i <- [minI .. maxI],
                                               j <- [minJ .. maxJ],
                                               k <- [minK .. maxK]]

getNeighbours :: A.Array IJK Bool -> IJK -> [IJK]
getNeighbours grid (i, j, k) = filter isInBound neighbours
   where
      ((minI, minJ, minK), (maxI, maxJ, maxK)) = A.bounds grid
      isInBound (i, j, k) = minI <= i && i <= maxI && minJ <= j && j <= maxJ && minK <= k && k <= maxK
      neighbours          = [(i + di, j + dj, k + dk) |
                             di <- [-1 .. 1],
                             dj <- [-1 .. 1],
                             dk <- [-1 .. 1],
                             abs di + abs dj + abs dk == 1]

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 = fst . foldl solve' (0, 0) . head
   where
      deltaJ (U v) = v
      deltaJ (D v) = -v
      deltaJ _     = 0
      solve' (ma, cur) dir = (ma', cur')
         where
            cur' = cur + deltaJ dir
            ma'  = max ma cur'

part2 :: Input -> Int
part2 = subtract 1 . S.length . foldr (\branch b -> b `S.union` segments branch) S.empty . solveBranches

part3 :: Input -> Int
part3 input = minimum $
   map    (\branch -> sum $ map (branch M.!) leafs) $
   filter (\branch -> all (`M.member` branch) leafs) $
   map    (\j -> solveTrunk ans (0, j, 0)) [minJ .. maxJ]

   where
      solveTrunk input start = bfs [start] visitedInitial
         where
            visitedInitial = M.fromList [(start, 0)]
            bfs [] visited = visited
            bfs (x : xs) visited = bfs xs' visited'
               where neighbours = getNeighbours input x
                     neighbours'= filter (`M.notMember` visited) neighbours
                     dist       = visited M.! x
                     accessible = filter (input A.!) neighbours'
                     xs'        = xs ++ accessible
                     visited'   = foldr (`M.insert` (dist + 1)) visited accessible

      branches  = solveBranches input
      segments' = S.elems $ foldr (S.union . segments) S.empty branches
      leafs     = map end branches

      maxI = maximum $ map (\(x, _, _) -> x) segments'
      minI = minimum $ map (\(x, _, _) -> x) segments'
      maxJ = maximum $ map (\(_, y, _) -> y) segments'
      minJ = minimum $ map (\(_, y, _) -> y) segments'
      maxK = maximum $ map (\(_, _, z) -> z) segments'
      minK = minimum $ map (\(_, _, z) -> z) segments'

      initial = A.listArray ((minI, minJ, minK), (maxI, maxJ, maxK)) $ repeat False
      ans     = initial A.// map (, True) segments'

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 7
   testCase part1 "part1" 159

   -- Part 2
   testCase part2 "part2_sample" 32
   testCase part2 "part2" 5049

   -- Part 3
   testCase part3 "part3_sample" 46
   testCase part3 "part3" 1488
