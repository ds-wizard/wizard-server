module Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts where

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.Project.Project
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Shared.Service.Project.ProjectMapper as ProjectMapper

wizardDocumentTemplateDraftData :: DocumentTemplateDraftData
wizardDocumentTemplateDraftData =
  DocumentTemplateDraftData
    { documentTemplateUuid = wizardDocumentTemplateDraft.uuid
    , projectUuid = Just project1.uuid
    , knowledgeModelEditorUuid = Nothing
    , formatUuid = Just formatJson.uuid
    , tenantUuid = wizardDocumentTemplateDraft.tenantUuid
    , createdAt = wizardDocumentTemplateDraft.createdAt
    , updatedAt = wizardDocumentTemplateDraft.updatedAt
    }

wizardDocumentTemplateDraftDataEdited :: DocumentTemplateDraftData
wizardDocumentTemplateDraftDataEdited =
  wizardDocumentTemplateDraftData
    { projectUuid = Just project2.uuid
    , formatUuid = Just formatPdf.uuid
    }

wizardDocumentTemplateDraftCreateDTO :: DocumentTemplateDraftCreateDTO
wizardDocumentTemplateDraftCreateDTO =
  DocumentTemplateDraftCreateDTO
    { name = "New Document Template"
    , templateId = wizardDocumentTemplateNlDraft.templateId
    , version = "3.0.0"
    , basedOn = Just wizardDocumentTemplateDraft.uuid
    }

wizardDocumentTemplateDraftChangeDTO :: DocumentTemplateDraftChangeDTO
wizardDocumentTemplateDraftChangeDTO = toChangeDTO wizardDocumentTemplateDraft

wizardDocumentTemplateDraftDataDTO :: DocumentTemplateDraftDataDTO
wizardDocumentTemplateDraftDataDTO =
  DocumentTemplateDraftDataDTO
    { projectUuid = wizardDocumentTemplateDraftDataEdited.projectUuid
    , project = Just . ProjectMapper.toSuggestion $ project2
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }

wizardDocumentTemplateDraftDataChangeDTO :: DocumentTemplateDraftDataChangeDTO
wizardDocumentTemplateDraftDataChangeDTO =
  DocumentTemplateDraftDataChangeDTO
    { projectUuid = wizardDocumentTemplateDraftDataEdited.projectUuid
    , knowledgeModelEditorUuid = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }
