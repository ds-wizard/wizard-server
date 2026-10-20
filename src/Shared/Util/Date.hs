module Shared.Util.Date where

import qualified Data.Attoparsec.Text as AT
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Parsers

-- First day in computer calendar
day0 = UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0

-- One Day represented in seconds
nominalDayInSeconds = 24 * 60 * 60

-- One Hour represented in seconds
nominalHourInSeconds = 60 * 60

dt' :: Integer -> Int -> Int -> UTCTime
dt' year month day = UTCTime (fromJust $ fromGregorianValid year month day) 0

dt'' :: Integer -> Int -> Int -> Integer -> UTCTime
dt'' year month day secsFromMidnight =
  UTCTime (fromJust $ fromGregorianValid year month day) (secondsToDiffTime secsFromMidnight)

parsePostgresDateTime :: String -> Either String UTCTime
parsePostgresDateTime = AT.parseOnly utcTime . T.pack

parsePostgresDateTime' :: String -> UTCTime
parsePostgresDateTime' dateTimeS =
  let (Right dateTime) = parsePostgresDateTime dateTimeS
   in dateTime

batchTimestamps :: UTCTime -> [UTCTime]
batchTimestamps start = fmap (\index -> addUTCTime (fromIntegral index * 0.000001) start) [0 :: Int ..]

getCurrentTimeInMicroseconds :: IO UTCTime
getCurrentTimeInMicroseconds = truncateToMicroseconds <$> getCurrentTime

truncateToMicroseconds :: UTCTime -> UTCTime
truncateToMicroseconds time = time {utctDayTime = picosecondsToDiffTime (diffTimeToPicoseconds (utctDayTime time) `div` 1000000 * 1000000)}
