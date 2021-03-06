{-|
Utility script for parsing UI test's framework.log and looking for log lines
that are followed by too large a delay.
Usage: analog <delay-in-seconds> </path/to/framework.log>
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Function (on)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Line = Text

main :: IO ()
main = do
   args <- getArgs
   case args of
       [delayStr, logfile] | Just delay <- readMaybe @Int delayStr
           -> analyzeLog delay logfile
       _   -> putStrLn "Usage: analog <delay-in-seconds> </path/to/framework.log>"

analyzeLog :: Int -> FilePath -> IO ()
analyzeLog delay f =
    Text.putStrLn
    . Text.unlines
    . linesWithDelayGreaterThan delay
    . calculateDelays
    . filter hasTime
    . Text.lines
    =<< Text.readFile f

hasTime :: Line -> Bool
hasTime l = Text.isPrefixOf "[" l && Text.isInfixOf "]" l

linesWithDelayGreaterThan :: Int -> [(NominalDiffTime, Line)] -> [Line]
linesWithDelayGreaterThan minDelay =
    map (\(d,l)-> "delay " <> Text.pack (show d) <> ": " <> l)
    . filter (\(d,_) -> d > fromIntegral minDelay)

-- For each line in the list calculates time it took for the following line to appear
calculateDelays :: [Line] -> [(NominalDiffTime, Line)]
calculateDelays ls =
    scanr getDiff seed ls
  where
    getDiff l1 (_, l2) = ((diffUTCTime `on` extractTime) l2 l1, l1)
    seed = (0 :: NominalDiffTime, head ls) --head ls = arbitrary line as a seed to start scanr

-- Line with time has the form of "[2017-02-24 12:26:38,273] DEBUG ..."
-- Need to parse the whole date for cases that log includes midnight
extractTime :: Line -> UTCTime
extractTime =
    parseTimeOrError True defaultTimeLocale "%F %T,%q"
    . Text.unpack
    . (<> "000000000") --apend 9 zeroes & parse as nanoseconds
    . Text.takeWhile (/=']') . Text.tail --extract part between '[' ']'
