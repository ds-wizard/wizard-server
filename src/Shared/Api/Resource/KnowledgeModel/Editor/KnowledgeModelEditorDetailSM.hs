module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailSM where

import Data.Swagger

import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorDetailDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorDetail
