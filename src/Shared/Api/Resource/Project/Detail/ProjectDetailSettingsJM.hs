module Shared.Api.Resource.Project.Detail.ProjectDetailSettingsJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectStateJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.Project.Detail.ProjectDetailSettings
import Shared.Util.Aeson

instance FromJSON ProjectDetailSettings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailSettings where
  toJSON = genericToJSON jsonOptions
