module Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteJM where

import Data.Aeson
import qualified Data.Text.Encoding as TE

import Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteDTO
import Shared.Util.JsonSchema

instance FromJSON McpAssetWriteDTO where
  parseJSON = withObject "asset" $ \o -> McpAssetWriteDTO <$> o .: "path" <*> (fmap TE.encodeUtf8 <$> o .:? "content") <*> o .:? "content_type" <*> o .:? "new_path"

instance JsonSchema McpAssetWriteDTO where
  jsonSchema _ =
    objectSchema
      ["path"]
      [ ("path", describedSchema "The path of the asset, for example src/logo.png" stringSchema)
      , ("content", describedSchema "The new content, base64 encoded; an asset that does not exist is created" stringSchema)
      , ("content_type", describedSchema "The content type, such as image/png; needed to create an asset" stringSchema)
      , ("new_path", describedSchema "The path to rename the asset to" stringSchema)
      ]
