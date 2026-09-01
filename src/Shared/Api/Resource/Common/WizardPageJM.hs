module Shared.Api.Resource.Common.WizardPageJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Shared.Model.Common.Page
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Util.JSON

instance FromJSON (Page KnowledgeModelEditorList) where
  parseJSON (Object o) = do
    page <- o .: "page"
    embedded <- o .: "_embedded" .-> "knowledgeModelEditors"
    return $ Page "knowledgeModelEditors" page embedded
  parseJSON _ = mzero
