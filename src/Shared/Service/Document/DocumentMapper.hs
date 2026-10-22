module Shared.Service.Document.DocumentMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.PersistentCommand
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.Document.DocumentList
import Shared.Model.Document.PreviewSource
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectSimple
import Shared.Model.Submission.SubmissionList
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Util.JSON
import Shared.Util.String (trim)

toDTO :: DocumentList -> [SubmissionList] -> DocumentDTO
toDTO doc submissions =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , project = Just ProjectSimple {uuid = doc.projectUuid, name = doc.projectName}
    , projectEventUuid = doc.projectEventUuid
    , projectVersion = doc.projectVersion
    , documentTemplate = doc.documentTemplate
    , format = doc.documentTemplateFormat
    , language = doc.language
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    , workspaceUuid = doc.workspaceUuid
    }

toDTOWithDocTemplate :: Document -> Project -> Maybe String -> [SubmissionList] -> DocumentTemplate -> DocumentTemplateFormatSimple -> DocumentDTO
toDTOWithDocTemplate doc project mProjectVersion submissions dt format =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , project = Just $ ProjectSimple {uuid = project.uuid, name = project.name}
    , projectEventUuid = doc.projectEventUuid
    , projectVersion = mProjectVersion
    , documentTemplate = toWithCoordinate dt
    , format = format
    , language = doc.language
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    , workspaceUuid = doc.workspaceUuid
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> Maybe String -> Maybe U.UUID -> Maybe UserDTO -> U.UUID -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid contextHash mProjectEventUuid mCurrentUser tenantUuid workspaceUuid now =
  Document
    { uuid = docUuid
    , name = trim dto.name
    , state = QueuedDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just dto.projectUuid
    , projectEventUuid = mProjectEventUuid
    , documentTemplateUuid = dto.documentTemplateUuid
    , formatUuid = dto.formatUuid
    , language = dto.language
    , createdBy = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , tenantUuid = tenantUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    , workspaceUuid = workspaceUuid
    , contextHash = contextHash
    , knowledgeModelEditorUuid = Nothing
    }

fromTemporallyCreateDTO :: U.UUID -> PreviewSource -> Maybe U.UUID -> U.UUID -> U.UUID -> Maybe String -> Maybe String -> Maybe UserDTO -> UTCTime -> Document
fromTemporallyCreateDTO docUuid source mProjectEventUuid documentTemplateUuid formatUuid language contextHash mCurrentUser now =
  Document
    { uuid = docUuid
    , name = previewSourceName source
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    , projectUuid =
        case source of
          ProjectPreviewSource project -> Just project.uuid
          KnowledgeModelEditorPreviewSource _ -> Nothing
    , projectEventUuid = mProjectEventUuid
    , documentTemplateUuid = documentTemplateUuid
    , formatUuid = formatUuid
    , language = language
    , createdBy = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , tenantUuid =
        case source of
          ProjectPreviewSource project -> project.tenantUuid
          KnowledgeModelEditorPreviewSource editor -> editor.tenantUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    , workspaceUuid =
        case source of
          ProjectPreviewSource project -> project.workspaceUuid
          KnowledgeModelEditorPreviewSource editor -> editor.workspaceUuid
    , contextHash = contextHash
    , knowledgeModelEditorUuid =
        case source of
          ProjectPreviewSource _ -> Nothing
          KnowledgeModelEditorPreviewSource editor -> Just editor.uuid
    }

previewSourceName :: PreviewSource -> String
previewSourceName (ProjectPreviewSource project) = trim project.name
previewSourceName (KnowledgeModelEditorPreviewSource editor) = trim editor.name

toDocPersistentCommand :: U.UUID -> DocumentContext -> Document -> PersistentCommand U.UUID
toDocPersistentCommand uuid docContext doc =
  toPersistentCommand
    uuid
    documentWorkerComponent
    "generateDocument"
    (encodeJsonToString docContext)
    10
    doc.tenantUuid
    doc.createdBy
    doc.createdAt
