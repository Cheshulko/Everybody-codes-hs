module Quest03.Solve (solve) where

import           Control.Exception
import           Data.Array
import           Data.List
import           Text.Printf

questPrefix = "app/Quest03/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Cell = Mine Int | None deriving (Show, Eq)

type Input = Array (Int, Int) Cell

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows = length content
   let cols = length . head $ content

   let lines' = map (\line -> '.' : line ++ ['.']) $
         [replicate cols '.'] ++ content ++ [replicate cols '.']

   let rows' = length lines'
   let cols' = length . head $ lines'
   let grid = map (map getCell) lines'

   return $ listArray ((1, 1), (rows', cols')) (concat grid)

getCell :: Char -> Cell
getCell '.' = None
getCell '#' = Mine 0

cellDepth :: Cell -> Int
cellDepth (Mine d) = d
cellDepth None     = 0

calcBlocks :: Input -> Int
calcBlocks grid = sum $ map cellDepth (elems grid)

mine :: (Int -> Bool) -> Input -> Input
mine neighboursFilter initialGrid = snd $ last $ takeWhile (uncurry (/=)) $ zip grids (tail grids)
   where
      grids = iterate (\grid ->
         grid // [((i, j), mine' grid (i, j)) | i <- [minRow..maxRow], j <- [minCol..maxCol], isPromising grid (i, j)])
         initialGrid
      isPromising grid index = case grid ! index of
         (Mine _) -> True
         _        -> False

      ((minRow, minCol), (maxRow, maxCol)) = bounds initialGrid
      currentDepth grid = cellDepth . (grid !)
      neighbours (i, j) = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], neighboursFilter (abs di + abs dj)]

      mine' grid index
         | case1 = Mine $ 1 + (cellDepth . (grid !)) (head accessible)
         | case2 = Mine $ maximum accessibleDepths
         | otherwise = grid ! index
         where
            case1 = differentNeighbours == 1
            case2 = differentNeighbours == 2 && currentDepth grid index == minimum accessibleDepths
            differentNeighbours = length . nub $ accessibleDepths
            accessible = neighbours index
            accessibleDepths = map (cellDepth . (grid !)) accessible

solvePart solver part = printf "Part `%s`: %d\n" part . solver =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 = calcBlocks . mine (== 1)

part2 :: Input -> Int
part2 = calcBlocks . mine (== 1)

part3 :: Input -> Int
part3 = calcBlocks . mine (/= 0)

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 35
   testCase part1 "part1" 124

   -- Part 2
   testCase part2 "part2" 2744

   -- Part 3
   testCase part3 "part3_sample" 29
   testCase part3 "part3" 10408
