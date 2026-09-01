module Shared.Api.Resource.Project.ProjectContentSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListSM ()
import Shared.Api.Resource.Project.Event.ProjectEventListSM ()
import Shared.Api.Resource.Project.ProjectContentDTO
import Shared.Api.Resource.Project.ProjectContentJM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListSM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectContentDTO where
  declareNamedSchema = toSwagger (toContentDTO project1Ctn M.empty [] [])
