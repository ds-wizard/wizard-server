module Shared.Api.Resource.Project.Detail.ProjectDetailPreviewJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Model.Project.Detail.ProjectDetailPreview
import Shared.Util.Aeson

instance FromJSON ProjectDetailPreview where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailPreview where
  toJSON = genericToJSON jsonOptions
