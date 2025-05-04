module Quest16.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.List         (group, intercalate, sort, transpose)
import           Data.List.Split   (splitOn)
import qualified Data.Map          as M
import           Text.Printf       (printf)

questPrefix = "app/Quest16/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Wheel = Wheel {
   getFaceDelta :: Int,
   getDelta     :: Int,
   getFaces     :: [String]
} deriving (Show, Eq, Ord)

type Input = [Wheel]

mapFaces :: String -> [String]
mapFaces (' ' : ' ' : ' ' : ' ' : xs) = "" : mapFaces xs
mapFaces [' ', ' ', ' ']              = [""]
mapFaces (x : y : z : ' ' : xs)       = [x, y, z] : mapFaces xs
mapFaces f                            = [f]

parseInput :: FilePath -> IO Input
parseInput filePath = do
   [deltas, facesList] <- splitOn "\n\n" <$> readFile filePath

   let deltas' = map read $ splitOn "," deltas
   let faces'  = map (filter (not . null)) $ transpose $ map mapFaces $ lines facesList

   return $ zipWith (Wheel 0) deltas' faces'

shiftLeft :: Int -> [a] -> [a]
shiftLeft n xs = end ++ start
   where
      n' = n `mod` length xs
      (start, end) = splitAt n' xs

calcPrize :: Input -> Int
calcPrize input = sum $ map (calc . length) chars
   where
      chars = group $ sort $ concatMap ((\[el, _, er] -> [el, er]) . head . getFaces) input
      calc cnt
         | cnt >= 3 = 1 + (cnt - 3)
         | otherwise = 0

pullWheel :: Int -> Wheel -> Wheel
pullWheel n wheel = wheel {getFaces = faces', getFaceDelta = faceDelta'}
   where
      len        = length faces
      delta      = getDelta wheel
      faces      = getFaces wheel
      faceDelta  = getFaceDelta wheel
      delta'     = (n * delta) `mod` len
      faces'     = shiftLeft delta' faces
      faceDelta' = (faceDelta + delta') `mod` len

cheatWheel :: Int -> Wheel -> Wheel
cheatWheel delta wheel = wheel {getFaces = faces', getFaceDelta = faceDelta'}
   where
      len        = length faces
      faces      = getFaces wheel
      faceDelta  = getFaceDelta wheel
      delta'     = (len + delta) `mod` len
      faces'     = shiftLeft delta' faces
      faceDelta' = (faceDelta + delta') `mod` len

playMachine :: Input -> Int -> Int
playMachine machine pulls = prize
   where
      (_, prize, _) = until (\(_, _, pulls') -> pulls' == pulls) playMachine' (machine, 0, 0)
         where
            playMachine' (machine, prize, pulls) = let
               nextMachine = map (pullWheel 1) machine
               in (nextMachine, prize + calcPrize nextMachine, pulls + 1)

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> String
part1 = unwords . map (head . getFaces . pullWheel 100)

part2 :: Input -> Int
part2 machine = (ans1 * x1) + ans2
   where
      pulls = 202420242024
      lcm'  = until (\value -> all (\wheel -> value * getDelta wheel `mod` length (getFaces wheel) == 0) machine) (+ 1) 1

      x1   = pulls `div` lcm'
      x2   = pulls `mod` lcm'
      ans1 = playMachine machine lcm'
      ans2 = playMachine machine x2

part3 :: Input -> String
part3 machine = unwords $ map show
   [
   maximum $ M.elems machinesMax,
   minimum $ M.elems machinesMin
   ]
   where
      pulls    = 256
      machines = M.fromList [(machine, 0)]

      (machinesMax, _) = until (\(_, pulls') -> pulls' == pulls) (play max) (machines, 0)
      (machinesMin, _) = until (\(_, pulls') -> pulls' == pulls) (play min) (machines, 0)

      play f (machines, pulls) =
         (
         foldr (\(k, v) -> M.alter (insertState v) k) M.empty $ concatMap playState $ M.assocs machines,
         pulls + 1
         )
         where
            playState (machine, prize) =
               [
               (cheatDown, prize + calcPrize cheatDown),
               (noCheat,   prize + calcPrize noCheat),
               (cheatUp,   prize + calcPrize cheatUp)
               ]
               where
                  cheatDown = map (pullWheel 1 . cheatWheel (-1)) machine
                  noCheat   = map (pullWheel 1) machine
                  cheatUp   = map (pullWheel 1 . cheatWheel 1) machine

            insertState value Nothing       = Just value
            insertState value (Just value') = Just $ f value value'

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" ">.- -.- ^,-"
   testCase part1 "part1" "^.- *,- -,> >,^"

   -- Part 2
   testCase part2 "part2_sample" 280014668134
   testCase part2 "part2" 124855474562

   -- Part 3
   testCase part3 "part3_sample" "627 128"
   testCase part3 "part3" "610 72"
