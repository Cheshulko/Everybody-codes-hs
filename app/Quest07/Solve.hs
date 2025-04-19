module Quest07.Solve (solve) where

import           Control.Exception           (assert)
import           Control.Parallel.Strategies
import           Data.Array
import           Data.List                   (permutations, sortOn)
import           Data.List.Split             (splitOn)
import qualified Data.Ord
import qualified Data.Set                    as S
import           Text.Printf                 (printf)

questPrefix = "app/Quest07/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Op = Increase | Decrease | Maintain deriving Show

getOp :: Char -> Op
getOp '+' = Increase
getOp '-' = Decrease
getOp '=' = Maintain
getOp 'S' = Maintain

getOpDelta :: Op -> Int
getOpDelta Increase = 1
getOpDelta Decrease = -1
getOpDelta Maintain = 0

type Plan = (String, [Op])
type Track = [Op]
type Input = ([Plan], Track)

shifted' :: [a] -> [a]
shifted' (x : xs) = xs ++ [x]

parsePlan :: String -> Plan
parsePlan line = (name, ops)
   where
      [name, remaining] = splitOn ":" line
      ops = map (getOp . head) $ splitOn "," remaining

parseTrack :: String -> Track
parseTrack "" = []
parseTrack track = shifted' $ map getOp (dfs grid start)
   where
      lines' = lines track
      rows   = length lines'
      cols   = length . head $ lines'
      grid   = listArray ((1, 1), (rows, cols)) (concat lines')
      start  = fst $ head $ filter ((== 'S') . snd) $ assocs grid

      dfs grid curPos = dfs' (accessible grid curPos)
         where
            ((minRow, minCol), (maxRow, maxCol)) = bounds grid
            isInBounds (i, j)                    = minRow <= i && i <= maxRow &&
                                                   minCol <= j && j <= maxCol
            neighbours (i, j)                    = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]
            accessible grid pos                  = filter (\pos -> isInBounds pos && (grid ! pos) /= ' ') $ neighbours pos

            dfs' children = case children of
               [] -> [cur]
               _  -> cur : dfs grid' curPos'
               where
                  cur     = grid ! curPos
                  curPos' = head children
                  grid'   = grid // [(curPos, ' ')]

parseInput :: FilePath -> IO Input
parseInput filePath = do
   [plan, track] <- splitOn "\n\n" <$> readFile filePath
   let content = lines plan

   return (map parsePlan content, parseTrack track)

applyTrackOp :: (Op, Op) -> Op
applyTrackOp (planOp, Maintain) = planOp
applyTrackOp (_, Increase)      = Increase
applyTrackOp (_, Decrease)      = Decrease

solveIteration :: Int -> Plan -> Track -> Int
solveIteration segments plan track = fst $ foldl acc' (0, segments) $ take segments $ zip (cycle ops) (cycle track)
   where
      ops                   = snd plan
      acc' (acc, cur) value = (acc + cur', cur')
         where
            cur' = cur + getOpDelta (applyTrackOp value)

solveSimple :: Int -> Track -> Input -> String
solveSimple segments track input = concatMap fst $ sortOn (Data.Ord.Down . snd) $ parMap rseq solvePlan plan
   where
      plan           = fst input
      solvePlan plan = (fst plan, solveIteration segments plan (cycle track))

solvePart solver part = printf "Part `%s`: %s\n" part . show . solver =<< parseInput (printf questPrefix part)

part1 :: Input -> String
part1 = solveSimple 10 [Maintain]

part2 :: Input -> String
part2 input = solveSimple (10 * length track) track input
   where
      track = snd input

part3 :: Input -> Int
part3 input = length solve'
   where
      segments     = 2024 * length track
      track        = snd input
      opponentPlan = fst input

      opponent = head $ map (solveIteration' segments (cycle track)) opponentPlan
      plans    = map (\op -> ("42", map getOp op)) $ S.elems $ foldr S.insert S.empty $ permutations "+++++---==="
      solve'   = filter (> opponent) $ parMap rseq (solveIteration' segments (cycle track)) plans

      solveIteration' :: Int -> Track -> Plan -> Int
      solveIteration' segments track plan = segments * segments + inc - dec
         where
            ops  = snd plan
            lcm' = lcm (length ops) (length track)
            cnt  = segments `div` lcm'

            calcProgression a = (2 * a + (cnt - 1) * lcm') * cnt `div` 2

            getOpDeltaOnSegment filterOp =
               sum $
               map (calcProgression . (\x -> lcm' - snd x)) $
               filter (filterOp . fst) $
               zip (map (getOpDelta . applyTrackOp) (take lcm' $ zip (cycle ops) (cycle track))) [0 .. lcm' - 1]

            inc = getOpDeltaOnSegment (== 1)
            dec = getOpDeltaOnSegment (== -1)

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" "BDCA"
   testCase part1 "part1" "BFGKIDCAH"

   -- Part 2
   testCase part2 "part2_sample" "DCBA"
   testCase part2 "part2" "IKGEDFAHC"

   -- Part 3
   testCase part3 "part3" 6492
