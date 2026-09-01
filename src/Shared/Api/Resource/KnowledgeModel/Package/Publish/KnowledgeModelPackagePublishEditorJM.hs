module Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Util.Aeson

instance FromJSON PackagePublishEditorDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishEditorDTO where
  toJSON = genericToJSON jsonOptions
