module Shared.Service.Mcp.Tool.McpToolCommon where

import Control.Monad ((<=<))
import Control.Monad.Reader (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser, parse)
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentDTO
import Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentJM ()
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.Mcp
import Shared.Database.DAO.Common (computeTotalPage, preparePaginationVariables)
import Shared.Localization.Locale
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Service.Workspace.WorkspaceScopeService (localScope)
import Shared.Util.JsonSchema

data Arguments a = Arguments
  { properties :: [(String, Value)]
  , required :: [String]
  , parser :: Object -> Parser a
  }

instance Functor Arguments where
  fmap f (Arguments properties required parser) = Arguments properties required (fmap f . parser)

instance Applicative Arguments where
  pure value = Arguments [] [] (const (pure value))
  Arguments properties1 required1 parser1 <*> Arguments properties2 required2 parser2 =
    Arguments (properties1 ++ properties2) (required1 ++ required2) (\o -> parser1 o <*> parser2 o)

readOnlyAnnotations :: McpToolAnnotations
readOnlyAnnotations = McpToolAnnotations {readOnlyHint = True, destructiveHint = False, idempotentHint = True, openWorldHint = False}

argumentsSchema :: Arguments a -> Value
argumentsSchema arguments = objectSchema arguments.required arguments.properties

withArguments :: Arguments a -> (a -> m Value) -> Value -> Either String (m Value)
withArguments arguments action value =
  case parse (withObject "arguments" arguments.parser) value of
    Success result -> Right (action result)
    Error message -> Left message

checkArguments :: (a -> Either LocaleRecord b) -> Arguments a -> Arguments b
checkArguments check arguments = Arguments arguments.properties arguments.required (either (fail . toLocaleMessage) return . check <=< arguments.parser)

withArgumentSchema :: Value -> Arguments a -> Arguments a
withArgumentSchema schema arguments = Arguments [(name, schema) | (name, _) <- arguments.properties] arguments.required arguments.parser

requiredArgument :: forall a. (FromJSON a, JsonSchema a) => String -> String -> Arguments a
requiredArgument name description =
  Arguments [(name, describedSchema description (jsonSchema (Proxy :: Proxy a)))] [name] $ \o ->
    argumentValue o name >>= maybe (fail . toLocaleMessage $ _ERROR_VALIDATION__MCP_MISSING_ARGUMENT name) return

optionalArgument :: forall a. (FromJSON a, JsonSchema a) => String -> String -> Arguments (Maybe a)
optionalArgument name description = Arguments [(name, describedSchema description (jsonSchema (Proxy :: Proxy a)))] [] (`argumentValue` name)

searchArguments :: String -> Arguments (Maybe String, Pageable)
searchArguments description = (,) <$> optionalArgument "q" description <*> pageableArguments

pageableArguments :: Arguments Pageable
pageableArguments = toPageable <$> optionalArgument "page" "Zero-based page number, 0 by default" <*> optionalArgument "size" "Page size, 20 by default"

scopeArguments :: WizardRequestContextC s m => Arguments (m a -> m a)
scopeArguments = uncurry localScope <$> scopeValueArguments

scopeValueArguments :: Arguments (Maybe U.UUID, Maybe Bool)
scopeValueArguments =
  (,)
    <$> optionalArgument "workspace_uuid" "The workspace to work in; leave it out to work across all workspaces you can reach"
    <*> optionalArgument "tenant" "Work at the organization level instead of in a workspace"

base64ContentArgument :: String -> String -> Arguments (Maybe BS.ByteString)
base64ContentArgument name description = checkArguments (traverse (decodeBase64Content name . TE.encodeUtf8)) (optionalArgument name description)

requiredBase64ContentArgument :: String -> String -> Arguments BS.ByteString
requiredBase64ContentArgument name description = checkArguments (decodeBase64Content name . TE.encodeUtf8) (requiredArgument name description)

textContentArgument :: Int -> String -> String -> Arguments (Maybe BS.ByteString)
textContentArgument limit name description = checkArguments (traverse (checkTextContent limit name)) (optionalArgument name description)

requiredTextContentArgument :: Int -> String -> String -> Arguments BS.ByteString
requiredTextContentArgument limit name description = checkArguments (checkTextContent limit name) (requiredArgument name description)

checkTextContent :: Int -> String -> T.Text -> Either LocaleRecord BS.ByteString
checkTextContent limit name content
  | T.length content > limit = Left $ _ERROR_VALIDATION__MCP_CONTENT_SIZE name limit
  | otherwise = Right (TE.encodeUtf8 content)

decodeBase64Content :: String -> BS.ByteString -> Either LocaleRecord BS.ByteString
decodeBase64Content name raw
  | BS.length encoded > (mcpMaxBase64ContentSize `div` 3 + 1) * 4 = Left $ _ERROR_VALIDATION__MCP_BASE64_SIZE name mcpMaxBase64ContentSize
  | otherwise = either (const . Left $ _ERROR_VALIDATION__MCP_BASE64 name) Right (convertFromBase Base64 encoded)
  where
    encoded = BS.filter (not . isSpace) (stripDataUrlPrefix raw)

stripDataUrlPrefix :: BS.ByteString -> BS.ByteString
stripDataUrlPrefix content
  | "data:" `BS.isPrefixOf` content, (_, rest) <- BS.breakSubstring ";base64," content, not (BS.null rest) = BS.drop 8 rest
  | otherwise = content

decodeText :: BS.ByteString -> String
decodeText = T.unpack . TE.decodeUtf8With lenientDecode

mcpTemporaryFile :: MonadIO m => (Int -> m TemporaryFileDTO) -> m McpTemporaryFileDTO
mcpTemporaryFile create = do
  expiresAt <- mcpUrlExpiresAt
  file <- create mcpTemporaryFileExpiration
  return $ McpTemporaryFileDTO file.url file.contentType expiresAt

mcpUrlExpiresAt :: MonadIO m => m UTCTime
mcpUrlExpiresAt = addUTCTime (fromIntegral mcpTemporaryFileExpiration) <$> liftIO getCurrentTime

toPageable :: Maybe McpNonNegativeArgumentDTO -> Maybe McpNonNegativeArgumentDTO -> Pageable
toPageable page size = Pageable (fmap (.number) page) (fmap (.number) size)

argumentValue :: FromJSON a => Object -> String -> Parser (Maybe a)
argumentValue o name = traverse parseArgument =<< o .:? K.fromString name
  where
    parseArgument value =
      case fromJSON value of
        Success result -> return result
        Error message -> fail . toLocaleMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT name message

toPage :: String -> Pageable -> [a] -> Page a
toPage name pageable entities =
  let (size, number, skip, limit) = preparePaginationVariables pageable
      total = length entities
   in Page name (PageMetadata size total (computeTotalPage total size) number) (take limit . drop skip $ entities)
