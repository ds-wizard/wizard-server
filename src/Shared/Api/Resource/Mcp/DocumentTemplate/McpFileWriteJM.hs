module Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteDTO
import Shared.Util.JsonSchema

instance FromJSON McpFileWriteDTO where
  parseJSON = withObject "file" $ \o -> McpFileWriteDTO <$> o .: "path" <*> o .:? "content" <*> o .:? "new_path"

instance JsonSchema McpFileWriteDTO where
  jsonSchema _ =
    objectSchema
      ["path"]
      [ ("path", describedSchema "The path of the file, or of a folder ending with /" stringSchema)
      , ("content", describedSchema "The new content of the file; a file that does not exist is created" stringSchema)
      , ("new_path", describedSchema "The path to rename the file, or move the folder, to" stringSchema)
      ]
