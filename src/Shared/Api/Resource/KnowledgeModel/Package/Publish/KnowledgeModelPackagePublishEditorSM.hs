module Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Util.Swagger

instance ToSchema PackagePublishEditorDTO where
  declareNamedSchema = toSwagger packagePublishEditorDTO
