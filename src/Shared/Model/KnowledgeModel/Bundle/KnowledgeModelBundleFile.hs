module Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile where

import qualified Data.ByteString.Lazy.Char8 as BSL

data KnowledgeModelBundleFile = KnowledgeModelBundleFile
  { fileName :: String
  , contentType :: String
  , content :: BSL.ByteString
  }
