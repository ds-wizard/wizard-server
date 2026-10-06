module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleFileJM where

import qualified Data.Text as T
import Servant.Multipart

import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile

instance FromMultipart Mem KnowledgeModelBundleFile where
  fromMultipart form =
    KnowledgeModelBundleFile
      <$> fmap (T.unpack . fdFileName) (lookupFile "file" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> fmap fdPayload (lookupFile "file" form)
