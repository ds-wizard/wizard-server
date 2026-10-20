module Shared.Util.WorkerLog where

import Data.Char (isDigit, toLower)
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Maybe (fromMaybe, listToMaybe)

import Shared.Util.List (lastSafe)

trimWorkerLog :: String -> String
trimWorkerLog workerLog =
  case lastSafe [index | (index, line) <- numbered, isErrorLine line] of
    Just errorIndex -> unlines . fmap snd . filter (\(index, _) -> index >= errorBlockStart errorIndex && index <= errorIndex) $ numbered
    Nothing -> unlines . reverse . take 20 . reverse . fmap snd $ numbered
  where
    numbered = zip [0 :: Int ..] (lines workerLog)
    errorBlockStart errorIndex =
      fromMaybe errorIndex (lastSafe [index | (index, line) <- numbered, index <= errorIndex, index >= errorIndex - 10, isLocationLine line])

isErrorLine :: String -> Bool
isErrorLine line = any (`isInfixOf` fmap toLower line) ["error", "exception"]

isLocationLine :: String -> Bool
isLocationLine line = any locationAt (tails (fmap toLower line))
  where
    locationAt rest = followedByDigit "line " rest || followedByDigit ".j2:" rest
    followedByDigit prefix rest = prefix `isPrefixOf` rest && maybe False isDigit (listToMaybe (drop (length prefix) rest))
