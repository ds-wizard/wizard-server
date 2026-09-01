module Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson

instance FromJSON ProjectDetailQuestionnaireDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailQuestionnaireDTO where
  toJSON = genericToJSON jsonOptions
