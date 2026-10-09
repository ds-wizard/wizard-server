module Shared.Util.JSON.Canonical where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Numeric (showHex)

encodeCanonical :: Value -> BSL.ByteString
encodeCanonical = B.toLazyByteString . build

build :: Value -> B.Builder
build Null = "null"
build (Bool True) = "true"
build (Bool False) = "false"
build (Number n) = B.string7 . formatNumber . toRealFloat $ n
build (String s) = buildString s
build (Array xs) = "[" <> commaSeparated (map build (V.toList xs)) <> "]"
build (Object o) = "{" <> commaSeparated (map buildMember (sortMembers o)) <> "}"

buildMember :: (Key, Value) -> B.Builder
buildMember (k, v) = buildString (K.toText k) <> ":" <> build v

sortMembers :: KM.KeyMap Value -> [(Key, Value)]
sortMembers = L.sortBy (comparing (TE.encodeUtf16BE . K.toText . fst)) . KM.toList

commaSeparated :: [B.Builder] -> B.Builder
commaSeparated = mconcat . L.intersperse ","

buildString :: T.Text -> B.Builder
buildString s = "\"" <> TE.encodeUtf8Builder (T.concatMap escapeChar s) <> "\""

escapeChar :: Char -> T.Text
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\b' = "\\b"
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar c
  | c < ' ' = T.pack $ "\\u" ++ replicate (4 - length hex) '0' ++ hex
  | otherwise = T.singleton c
  where
    hex = showHex (fromEnum c) ""

formatNumber :: Double -> String
formatNumber d
  | d == 0 = "0"
  | d < 0 = '-' : formatPositive (negate d)
  | otherwise = formatPositive d

formatPositive :: Double -> String
formatPositive d
  | k <= n && n <= 21 = digits ++ replicate (n - k) '0'
  | 0 < n && n <= 21 = take n digits ++ "." ++ drop n digits
  | -6 < n && n <= 0 = "0." ++ replicate (negate n) '0' ++ digits
  | otherwise = mantissa ++ "e" ++ expSign ++ show (abs (n - 1))
  where
    (ds, n) = shortestDigits d
    digits = concatMap show ds
    k = length digits
    expSign = if n - 1 >= 0 then "+" else "-"
    mantissa = case digits of
      [c] -> [c]
      (c : rest) -> c : '.' : rest
      [] -> "0"

shortestDigits :: Double -> ([Int], Int)
shortestDigits d
  | admissible (10 ^^ k) = ([1], k + 1)
  | otherwise = (generateDigits inclusive (v / 10 ^^ k) (halfDown / 10 ^^ k) (halfUp / 10 ^^ k), k)
  where
    (f, e) = decodeSubnormal d
    v = toRational d
    k = decimalExponent v
    inclusive = even f
    halfUp = 2 ^^ (e - 1) :: Rational
    halfDown = if f == 2 ^ (floatDigits d - 1) && e > minExponent d then 2 ^^ (e - 2) else halfUp
    admissible x = if inclusive then v + halfUp >= x else v + halfUp > x

decodeSubnormal :: Double -> (Integer, Int)
decodeSubnormal d
  | e < minExponent d = (f `div` 2 ^ (minExponent d - e), minExponent d)
  | otherwise = (f, e)
  where
    (f, e) = decodeFloat d

minExponent :: Double -> Int
minExponent d = fst (floatRange d) - floatDigits d

decimalExponent :: Rational -> Int
decimalExponent v = adjust (ceiling (logBase 10 (fromRational v :: Double)))
  where
    adjust k
      | 10 ^^ (k - 1) > v = adjust (k - 1)
      | 10 ^^ k <= v = adjust (k + 1)
      | otherwise = k

generateDigits :: Bool -> Rational -> Rational -> Rational -> [Int]
generateDigits inclusive r mDown mUp =
  case (low, high) of
    (False, False) -> d : generateDigits inclusive r' mDown' mUp'
    (True, False) -> [d]
    (False, True) -> [d + 1]
    (True, True) -> [if r' * 2 < 1 || (r' * 2 == 1 && even d) then d else d + 1]
  where
    r10 = r * 10
    d = floor r10 :: Int
    r' = r10 - fromIntegral d
    mDown' = mDown * 10
    mUp' = mUp * 10
    low = if inclusive then r' <= mDown' else r' < mDown'
    high = if inclusive then r' + mUp' >= 1 else r' + mUp' > 1
