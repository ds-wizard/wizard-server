module Shared.Service.Document.Context.DocumentContextHash where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KM

import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Util.Crypto (sha256Hex)
import Shared.Util.JSON.Canonical (encodeCanonical)

computeContextHash :: DocumentContext -> String
computeContextHash = computeContextValueHash . toJSON

computeContextValueHash :: Value -> String
computeContextValueHash value = "sha256:" <> sha256Hex (encodeCanonical (stripForHash value))

stripForHash :: Value -> Value
stripForHash (Object o) = Object . KM.delete "hash" . KM.delete "document" . stripKnowledgeModel $ o
  where
    stripKnowledgeModel obj =
      case KM.lookup "project" obj of
        Just Null -> obj
        Just _ -> KM.delete "knowledgeModel" obj
        Nothing -> obj
stripForHash value = value
