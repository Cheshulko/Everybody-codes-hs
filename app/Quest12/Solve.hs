{-# LANGUAGE TupleSections #-}
module Quest12.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.List         (sort)
import           Data.Maybe        (catMaybes, mapMaybe)
import           Text.Printf       (printf)

questPrefix = "app/Quest12/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Grid = Grid {
   grid     :: A.Array (Int, Int) Char,
   targets  :: [(Int, Int)],
   segments :: [(Int, Int)]
} deriving Show

type Meteors = [(Int, Int)]

data Input = T12 Grid | T3 Meteors deriving Show

getSegmentNumber :: Char -> Int
getSegmentNumber 'A' = 1
getSegmentNumber 'B' = 2
getSegmentNumber 'C' = 3

getTargetDurability :: Char -> Int
getTargetDurability 'T' = 1
getTargetDurability 'H' = 2

parseInput3 :: FilePath -> IO Input
parseInput3 filePath = T3 . map ((\[x, y] -> (read x, read y)) . words) . lines <$> readFile filePath

parseInput :: String -> IO Input
parseInput part@"part3_sample" = parseInput3 $ printf questPrefix part
parseInput part@"part3"        = parseInput3 $ printf questPrefix part
parseInput part = do
   content <- lines <$> readFile (printf questPrefix part)

   let rows' = length content
   let cols' = length . head $ content
   let grid  = A.listArray ((1, 1), (rows', cols')) (concat content)

   let targets  = map fst $ filter ((`elem` ['T', 'H']) . snd) $ A.assocs grid
   let segments = map fst $ filter ((`elem` ['A', 'B', 'C']) . snd) $ A.assocs grid

   return $ T12 Grid {
      grid     = grid,
      targets  = targets,
      segments = segments
   }

calcPower :: (Int, Int) -> (Int, Int) -> Maybe Int
calcPower (y2, x2) (y1, x1) = last $ Nothing : map Just (catMaybes [case1, case2])
   where
      dist = x2 - x1 - y2 + y1

      case1
         | dist `mod` 3 == 0 && y1 - power <= y2 && x1 + 2 * power <= x2 = Just power
         | otherwise = Nothing
         where
            power = dist `div` 3

      case2
         | power >= 0 && dx >= 0 && dx <= power = Just power
         | otherwise = Nothing
         where
            power = y1 - y2
            dx    = x2 + x1 - power

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput part

part1 :: Input -> Int
part1 (T12 input) = sum $ map (\target -> targetDurability target * minimumPower target) $ targets input
   where
      gridAt              = (grid input A.!)
      segmentNumber       = getSegmentNumber . gridAt
      targetDurability    = getTargetDurability . gridAt
      minimumPower target = minimum [power * segmentNumber segPos |
                                     segPos <- segments input,
                                     Just power <- [calcPower target segPos]]

part2 :: Input -> Int
part2 = part1

part3 :: Input -> Int
part3 (T3 input) = sum $ map ((snd . head) . solveMeteor) input
   where
      solveMeteor pos = sort $ concatMap (solveSegment pos) [0 .. 2]

      solveSegment (x, y) segmentInd =
         map (\(y, power) -> (y, power * (1 + segmentInd)) ) $
         mapMaybe (\(y, power) -> (y, ) <$> power) [solveShotTimed time |
                                                    time <- [0 .. (min x y)],
                                                    x - time <= time]
         where
            solveShotTimed time = (-y + time, calcPower (-y + time, x - time) (-segmentInd, 0))

testCase solver part result = do
   x <- solver <$> parseInput part
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 13
   testCase part1 "part1" 205

   -- Part 2
   testCase part2 "part2_sample" 22
   testCase part2 "part2" 19366

   -- Part 3
   testCase part3 "part3_sample" 11
   testCase part3 "part3" 730200
