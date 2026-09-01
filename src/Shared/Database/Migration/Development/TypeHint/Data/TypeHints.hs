module Shared.Database.Migration.Development.TypeHint.Data.TypeHints where

import Data.Aeson
import qualified Data.Map.Strict as M

import Shared.Api.Resource.TypeHint.TypeHintRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Project

forestDatasetTypeHint :: TypeHintIDTO
forestDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000001: Forest Dataset"
    , value = "Forest Dataset (biology)"
    , raw = object ["id" .= "r000001", "name" .= "Forest Dataset", "domain" .= "biology", "country" .= "cz"]
    }

genomicDatasetTypeHint :: TypeHintIDTO
genomicDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000002: Genomic Dataset"
    , value = "Genomic Dataset (biology)"
    , raw = object ["id" .= "r000002", "name" .= "Genomic Dataset", "domain" .= "biology", "country" .= "cz"]
    }

animalsDatasetTypeHint :: TypeHintIDTO
animalsDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000003: Animals Dataset"
    , value = "Animals Dataset (biology)"
    , raw = object ["id" .= "r000003", "name" .= "Animals Dataset", "domain" .= "biology", "country" .= "cz"]
    }

kmEditorIntegrationTypeHintRequest :: TypeHintRequestDTO
kmEditorIntegrationTypeHintRequest = KnowledgeModelEditorIntegrationTypeHintRequest' kmEditorIntegrationTypeHintRequest'

kmEditorQuestionTypeHintRequest :: TypeHintRequestDTO
kmEditorQuestionTypeHintRequest = KnowledgeModelEditorQuestionTypeHintRequest' kmEditorQuestionTypeHintRequest'

projectTypeHintRequest :: TypeHintRequestDTO
projectTypeHintRequest = ProjectTypeHintRequest' projectTypeHintRequest'

kmEditorIntegrationTypeHintRequest' :: KnowledgeModelEditorIntegrationTypeHintRequest
kmEditorIntegrationTypeHintRequest' =
  KnowledgeModelEditorIntegrationTypeHintRequest
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , integrationUuid = repositoryApi.uuid
    }

kmEditorQuestionTypeHintRequest' :: KnowledgeModelEditorQuestionTypeHintRequest
kmEditorQuestionTypeHintRequest' =
  KnowledgeModelEditorQuestionTypeHintRequest
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

projectTypeHintRequest' :: ProjectTypeHintRequest
projectTypeHintRequest' =
  ProjectTypeHintRequest
    { projectUuid = project15.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

typeHintTestRequest :: TypeHintTestRequestDTO
typeHintTestRequest =
  TypeHintTestRequestDTO
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , integrationUuid = repositoryApi.uuid
    , variables = M.fromList [("domain", "biology"), ("country", "cz")]
    , q = "biology"
    }
