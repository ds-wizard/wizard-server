module Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Util.Swagger

instance ToSchema SetRepliesDTO where
  declareNamedSchema = toSwagger setRepliesDTO
