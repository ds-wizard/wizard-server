module Shared.Util.Base64 where

import Data.ByteArray.Encoding (Base (..), convertFromBase)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)

import Shared.Localization.Messages.Public
import Shared.Model.Localization.LocaleRecord

decodeBase64WithLimit :: Int -> BS.ByteString -> Either LocaleRecord BS.ByteString
decodeBase64WithLimit limit raw
  | BS.length encoded > (limit `div` 3 + 1) * 4 = Left $ _ERROR_UTIL_BASE64__TOO_LARGE limit
  | otherwise = either (const . Left $ _ERROR_UTIL_BASE64__INVALID) Right (convertFromBase Base64 encoded)
  where
    encoded = BS.filter (not . isSpace) (stripDataUrlPrefix raw)

stripDataUrlPrefix :: BS.ByteString -> BS.ByteString
stripDataUrlPrefix content
  | "data:" `BS.isPrefixOf` content, (_, rest) <- BS.breakSubstring ";base64," content, not (BS.null rest) = BS.drop 8 rest
  | otherwise = content
