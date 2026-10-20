module Shared.Api.Resource.Common.PageJM where

import Data.Aeson
import Data.Proxy
import Data.String (fromString)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Util.JsonSchema

instance ToJSON entity => ToJSON (Page entity) where
  toJSON (Page name page entities) = object ["page" .= page, "_embedded" .= object [fromString name .= entities]]

pageSchema :: String -> Value -> Value
pageSchema name entitySchema =
  objectSchema
    ["page", "_embedded"]
    [ ("page", jsonSchema (Proxy :: Proxy PageMetadata))
    , ("_embedded", objectSchema [name] [(name, arraySchema entitySchema)])
    ]
