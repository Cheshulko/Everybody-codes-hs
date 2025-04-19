module Main where

import           System.Environment
import           Text.Printf

import           Control.Concurrent (setNumCapabilities)
import qualified Quest01.Solve      as Q01
import qualified Quest02.Solve      as Q02
import qualified Quest03.Solve      as Q03
import qualified Quest04.Solve      as Q04
import qualified Quest05.Solve      as Q05
import qualified Quest06.Solve      as Q06
import qualified Quest07.Solve      as Q07

solve :: String -> String -> IO ()
solve "quest01" part = Q01.solve part
solve "quest02" part = Q02.solve part
solve "quest03" part = Q03.solve part
solve "quest04" part = Q04.solve part
solve "quest05" part = Q05.solve part
solve "quest06" part = Q06.solve part
solve "quest07" part = Q07.solve part

solve quest part     = printf "Wrong input: quest=`%s` part=`%s`\n" quest part

main :: IO ()
main = do
    setNumCapabilities 128
    quest:part:_ <- getArgs
    solve quest part
