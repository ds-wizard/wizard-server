module Shared.Service.Mcp.Server.McpOutputLimit where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (..), convertFromBase, convertToBase)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V

import Shared.Constant.Mcp
import Shared.Localization.Locale
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Mcp.McpCursor
import Shared.Util.JsonSchema (describedSchema, stringSchema, typeSchema)

type Fits = Value -> Maybe McpCursor -> Bool

type Cut = (Value, Maybe McpCursor)

cursorProperty :: (String, Value)
cursorProperty = ("cursor", describedSchema "The cursor of a truncated result of this tool; repeat the same arguments with it to get the rest" stringSchema)

truncatedProperty :: (String, Value)
truncatedProperty = ("truncated", describedSchema "Set when the result was cut to fit; call the tool again with the cursor to get the rest" (typeSchema "boolean"))

splitCursorArgument :: Value -> Either String (Maybe McpCursor, Value)
splitCursorArgument (Object arguments) =
  case KM.lookup "cursor" arguments of
    Nothing -> Right (Nothing, Object arguments)
    Just (String text) | Just cursor <- decodeCursor (T.unpack text) -> Right (Just cursor, Object (KM.delete "cursor" arguments))
    Just _ -> Left (toLocaleMessage _ERROR_VALIDATION__MCP_INVALID_CURSOR)
splitCursorArgument arguments = Right (Nothing, arguments)

limitOutput :: Maybe McpCursor -> Value -> Either String Value
limitOutput mCursor result = do
  let start = fromMaybe (McpCursor [] 0) mCursor
  (page, mNext) <- maybe (Left (toLocaleMessage _ERROR_VALIDATION__MCP_INVALID_CURSOR)) Right (cutAt fitsPage start.path start.offset result)
  let output = maybe page (`markTruncated` page) mNext
  if fitsOutput output then Right output else Left (toLocaleMessage _ERROR_SERVICE_MCP__OUTPUT_TOO_LARGE)

cutAt :: Fits -> [McpCursorStep] -> Int -> Value -> Maybe Cut
cutAt fits (KeyMcpCursorStep key : rest) offset (Object o) = do
  child <- KM.lookup (K.fromString key) o
  let rebuild page = Object (KM.insert (K.fromString key) page o)
  let step = KeyMcpCursorStep key
  (page, mNext) <- cutAt (\page' mNext' -> fits (rebuild page') (stepInto step <$> mNext')) rest offset child
  return (rebuild page, stepInto step <$> mNext)
cutAt fits [] 0 (Object o) = Just (cutObject fits o)
cutAt fits path offset (Object o) = first fromEntries <$> cutAt (fits . fromEntries) path offset (toEntries o)
cutAt fits (IndexMcpCursorStep start : rest) offset (Array items) = do
  item <- items V.!? start
  (itemRest, _) <- cutAt (\_ _ -> True) rest offset item
  return $ cutItems fits start (V.cons itemRest (V.drop (start + 1) items)) (\fits' -> cutAt fits' rest offset item)
cutAt fits [] offset (Array items)
  | offset <= V.length items = Just $ cutItems fits offset (V.drop offset items) (\fits' -> cutAt fits' [] 0 =<< items V.!? offset)
cutAt fits [] offset (String text)
  | offset <= T.length text = Just $ cutText fits offset (T.drop offset text)
cutAt _ [] 0 value = Just (value, Nothing)
cutAt _ _ _ _ = Nothing

cutObject :: Fits -> Object -> Cut
cutObject fits o
  | fits (Object o) Nothing = (Object o, Nothing)
  | Just cut <- cutLargestChild fits o, uncurry fits cut = cut
  | KM.size o > 1 = maybe (Object o, Nothing) (first fromEntries) (cutAt (fits . fromEntries) [] 0 (toEntries o))
  | otherwise = fromMaybe (Object o, Nothing) (cutLargestChild fits o)

cutLargestChild :: Fits -> Object -> Maybe Cut
cutLargestChild fits o = (\key -> cutAt fits [KeyMcpCursorStep (K.toString key)] 0 (Object o)) =<< largestChild o

cutItems :: Fits -> Int -> V.Vector Value -> (Fits -> Maybe Cut) -> Cut
cutItems fits start items cutFirst
  | fits (Array items) Nothing = (Array items, Nothing)
  | count > 0 = (Array (V.take count items), Just (McpCursor [] (start + count)))
  | otherwise = maybe (Array items, Nothing) withFirst (cutFirst (\page mNext -> fits (Array (V.singleton page)) (Just (afterFirst mNext))))
  where
    count = largestFitting (\n -> fits (Array (V.take n items)) (Just (McpCursor [] (start + n)))) (V.length items - 1)
    withFirst (page, mNext) = (Array (V.singleton page), Just (afterFirst mNext))
    afterFirst = maybe (McpCursor [] (start + 1)) (stepInto (IndexMcpCursorStep start))

cutText :: Fits -> Int -> T.Text -> Cut
cutText fits offset text
  | fits (String text) Nothing = (String text, Nothing)
  | count > 0 = (String (T.take count text), Just (McpCursor [] (offset + count)))
  | otherwise = (String text, Nothing)
  where
    count = largestFitting (\n -> fits (String (T.take n text)) (Just (McpCursor [] (offset + n)))) (T.length text - 1)

largestFitting :: (Int -> Bool) -> Int -> Int
largestFitting fits = go 0
  where
    go low high
      | low >= high = low
      | fits middle = go middle high
      | otherwise = go low (middle - 1)
      where
        middle = (low + high + 1) `div` 2

largestChild :: Object -> Maybe K.Key
largestChild o =
  case [(key, encodedSize child) | (key, child) <- KM.toList o, isCuttable child] of
    [] -> Nothing
    sizes -> Just . fst $ maximumBy (comparing snd) sizes
  where
    isCuttable (Object _) = True
    isCuttable (Array _) = True
    isCuttable (String _) = True
    isCuttable _ = False

toEntries :: Object -> Value
toEntries o = Array . V.fromList $ fmap (\(key, value) -> Object (KM.singleton key value)) (KM.toAscList o)

fromEntries :: Value -> Value
fromEntries (Array entries) = Object $ foldr KM.union KM.empty [o | Object o <- V.toList entries]
fromEntries value = value

stepInto :: McpCursorStep -> McpCursor -> McpCursor
stepInto step cursor = cursor {path = step : cursor.path}

fitsPage :: Fits
fitsPage page mNext = fitsOutput (maybe page (`markTruncated` page) mNext)

fitsOutput :: Value -> Bool
fitsOutput = (<= mcpMaxOutputSize) . encodedSize . toToolResult

markTruncated :: McpCursor -> Value -> Value
markTruncated cursor (Object o) = Object . KM.insert "truncated" (Bool True) . KM.insert "cursor" (toJSON (encodeCursor cursor)) $ o
markTruncated cursor value = markTruncated cursor (object ["result" .= value])

toToolResult :: Value -> Value
toToolResult result = object ["content" .= [textContent (encodeText result)], "structuredContent" .= result]

toToolError :: String -> Value
toToolError = errorResult . limitErrorMessage

errorResult :: String -> Value
errorResult message = object ["content" .= [textContent (T.pack message)], "isError" .= True]

textContent :: T.Text -> Value
textContent text = object ["type" .= ("text" :: String), "text" .= text]

encodeText :: Value -> T.Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

encodedSize :: Value -> Int
encodedSize = fromIntegral . BSL.length . encode

encodeCursor :: McpCursor -> String
encodeCursor = BS.unpack . convertToBase Base64URLUnpadded . BSL.toStrict . encode

decodeCursor :: String -> Maybe McpCursor
decodeCursor text = either (const Nothing) decodeStrict (convertFromBase Base64URLUnpadded (BS.pack text) :: Either String BS.ByteString)

limitErrorMessage :: String -> String
limitErrorMessage message
  | fitsError message = message
  | otherwise = take count message ++ truncatedErrorSuffix
  where
    count = largestFitting (\n -> fitsError (take n message ++ truncatedErrorSuffix)) (length message)
    fitsError text = encodedSize (errorResult text) <= mcpMaxOutputSize

truncatedErrorSuffix :: String
truncatedErrorSuffix = " ... (cut at the size limit of the tool output)"

withoutCursor :: Value -> Value
withoutCursor (Object o) = Object (KM.delete "cursor" o)
withoutCursor value = value
