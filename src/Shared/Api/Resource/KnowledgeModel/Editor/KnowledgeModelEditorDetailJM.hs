module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM where

import Data.Aeson

import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelEditorDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorDetailDTO where
  toJSON = genericToJSON jsonOptions
