module Main where

import           System.Environment
import           Text.Printf

import qualified Quest01.Solve      as Q01
import qualified Quest02.Solve      as Q02
import qualified Quest03.Solve      as Q03
import qualified Quest04.Solve      as Q04

solve :: String -> String -> IO ()
solve "quest01" part = Q01.solve part
solve "quest02" part = Q02.solve part
solve "quest03" part = Q03.solve part
solve "quest04" part = Q04.solve part

solve quest part     = printf "Wrong input: quest=`%s` part=`%s`\n" quest part

main :: IO ()
main = do
    quest:part:_ <- getArgs
    solve quest part
