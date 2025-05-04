module Main where

import           System.Environment (getArgs)
import           Text.Printf        (printf)

import           Control.Concurrent (setNumCapabilities)
import qualified Quest01.Solve      as Q01
import qualified Quest02.Solve      as Q02
import qualified Quest03.Solve      as Q03
import qualified Quest04.Solve      as Q04
import qualified Quest05.Solve      as Q05
import qualified Quest06.Solve      as Q06
import qualified Quest07.Solve      as Q07
import qualified Quest08.Solve      as Q08
import qualified Quest09.Solve      as Q09
import qualified Quest10.Solve      as Q10
import qualified Quest11.Solve      as Q11
import qualified Quest12.Solve      as Q12
import qualified Quest13.Solve      as Q13
import qualified Quest14.Solve      as Q14
import qualified Quest15.Solve      as Q15
import qualified Quest16.Solve      as Q16

solve :: String -> String -> IO ()
solve "quest01" part = Q01.solve part
solve "quest02" part = Q02.solve part
solve "quest03" part = Q03.solve part
solve "quest04" part = Q04.solve part
solve "quest05" part = Q05.solve part
solve "quest06" part = Q06.solve part
solve "quest07" part = Q07.solve part
solve "quest08" part = Q08.solve part
solve "quest09" part = Q09.solve part
solve "quest10" part = Q10.solve part
solve "quest11" part = Q11.solve part
solve "quest12" part = Q12.solve part
solve "quest13" part = Q13.solve part
solve "quest14" part = Q14.solve part
solve "quest15" part = Q15.solve part
solve "quest16" part = Q16.solve part

solve quest part     = printf "Wrong input: quest=`%s` part=`%s`\n" quest part

main :: IO ()
main = do
    setNumCapabilities 128
    quest:part:_ <- getArgs
    solve quest part
