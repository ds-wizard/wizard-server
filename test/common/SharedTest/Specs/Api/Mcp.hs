module SharedTest.Specs.Api.Mcp where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.JSON.JSONSchema (validate)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Network.HTTP.Types (Header, methodPost, renderSimpleQuery)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai (WaiSession, liftIO, request)
import Test.Hspec.Wai.Matcher

import Shared.Constant.Mcp
import Shared.Model.Localization.LocaleRecord
import Shared.Util.String (f')

formBody :: [(String, String)] -> BSL.ByteString
formBody = BSL.fromStrict . renderSimpleQuery False . fmap (bimap BS.pack BS.pack)

valueField :: String -> Value -> Maybe Value
valueField name (Object o) = KM.lookup (K.fromString name) o
valueField _ _ = Nothing

resultField :: String -> Value -> Maybe Value
resultField name body = valueField name =<< valueField "result" body

localeMessage :: LocaleRecord -> String
localeMessage record = f' record.defaultMessage record.params

rpcBody :: String -> Value -> BSL.ByteString
rpcBody method params = encode $ object ["jsonrpc" .= ("2.0" :: String), "id" .= (1 :: Int), "method" .= method, "params" .= params]

toolCallBody :: String -> Value -> BSL.ByteString
toolCallBody name arguments = rpcBody "tools/call" (object ["name" .= name, "arguments" .= arguments])

rpcResult :: Value -> Value
rpcResult result = object ["jsonrpc" .= ("2.0" :: String), "id" .= (1 :: Int), "result" .= result]

rpcError :: Int -> String -> Value
rpcError code message = object ["jsonrpc" .= ("2.0" :: String), "id" .= (1 :: Int), "error" .= object ["code" .= code, "message" .= message]]

initializeResult :: String -> Value
initializeResult protocolVersion =
  rpcResult $
    object
      [ "protocolVersion" .= protocolVersion
      , "capabilities" .= object ["tools" .= object []]
      , "serverInfo" .= object ["name" .= ("FAIR Wizard" :: String), "version" .= ("1.0.0" :: String)]
      , "instructions" .= mcpInstructions
      ]

toolResult :: Value -> Value
toolResult structuredContent =
  rpcResult $
    object
      [ "content" .= [object ["type" .= ("text" :: String), "text" .= (TL.unpack . TLE.decodeUtf8 $ encode structuredContent)]]
      , "structuredContent" .= structuredContent
      ]

toolErrorResult :: String -> Value
toolErrorResult message = rpcResult $ object ["content" .= [object ["type" .= ("text" :: String), "text" .= message]], "isError" .= True]

readOnlyAnnotations :: Value
readOnlyAnnotations = object ["readOnlyHint" .= True, "destructiveHint" .= False, "idempotentHint" .= True, "openWorldHint" .= False]

bodyEqualsJson :: Value -> MatchBody
bodyEqualsJson expected =
  MatchBody $ \_ body ->
    if decode body == Just expected
      then Nothing
      else Just $ "body mismatch:\n  expected: " ++ BSL.unpack (encode expected) ++ "\n  but got:  " ++ BSL.unpack body

structuredContentOf :: SResponse -> Maybe Value
structuredContentOf response = resultField "structuredContent" =<< decode (simpleBody response)

stringField :: String -> Value -> Maybe String
stringField name value =
  case valueField name value of
    Just (String text) -> Just (T.unpack text)
    _ -> Nothing

confirmationTokenOf :: SResponse -> Maybe String
confirmationTokenOf response = stringField "confirmation_token" =<< structuredContentOf response

toolErrorOf :: SResponse -> Maybe Value
toolErrorOf response = resultField "isError" =<< decode (simpleBody response)

toolsOf :: SResponse -> [Value]
toolsOf response =
  case resultField "tools" =<< decode (simpleBody response) of
    Just (Array tools) -> V.toList tools
    _ -> []

assertToolOutputSchema :: [Header] -> String -> Value -> WaiSession st ()
assertToolOutputSchema headers name structuredContent = do
  response <- request methodPost "/api/mcp" headers (rpcBody "tools/list" (object []))
  let outputSchema = valueField "outputSchema" =<< L.find ((== Just (String (T.pack name))) . valueField "name") (toolsOf response)
  liftIO $ fmap (`validate` structuredContent) outputSchema `shouldBe` Just (Right ())
