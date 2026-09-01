module Shared.Service.Document.DocumentMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.KnowledgeModel
import Shared.Model.Common.Lens
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.Document.DocumentList
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectSimple
import Shared.Model.Submission.SubmissionList
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Util.JSON
import Shared.Util.List
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
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> Int -> [ProjectEventList] -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid repliesHash projectEvents mCurrentUser tenantUuid now =
  Document
    { uuid = docUuid
    , name = trim dto.name
    , state = QueuedDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just dto.projectUuid
    , projectEventUuid =
        case dto.projectEventUuid of
          Just projectEventUuid -> Just projectEventUuid
          Nothing -> fmap getUuid (lastSafe projectEvents)
    , projectRepliesHash = repliesHash
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
    }

fromTemporallyCreateDTO :: U.UUID -> Project -> Maybe U.UUID -> U.UUID -> U.UUID -> Maybe String -> Int -> Maybe UserDTO -> U.UUID -> UTCTime -> Bool -> Document
fromTemporallyCreateDTO docUuid project projectEventUuid documentTemplateUuid formatUuid language repliesHash mCurrentUser tenantUuid now fromKnowledgeModelEditor =
  Document
    { uuid = docUuid
    , name = trim project.name
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    , projectUuid =
        if fromKnowledgeModelEditor
          then Nothing
          else Just project.uuid
    , projectEventUuid = projectEventUuid
    , projectRepliesHash = repliesHash
    , documentTemplateUuid = documentTemplateUuid
    , formatUuid = formatUuid
    , language = language
    , createdBy = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , tenantUuid = tenantUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    }

toTemporaryPackage :: U.UUID -> UTCTime -> KnowledgeModelPackage
toTemporaryPackage tenantUuid createdAt =
  KnowledgeModelPackage
    { uuid = U.nil
    , name = "Example Knowledge Model"
    , organizationId = "org.example"
    , kmId = "km-example"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = knowledgeModelMetamodelVersion
    , description = "Example description"
    , readme = "# Example Knowledge Model\n\nThis is an example knowledge model."
    , license = "Apache-2.0"
    , language = "en"
    , previousPackageUuid = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , public = False
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    }

toTemporaryProject :: KnowledgeModelEditor -> KnowledgeModelPackage -> Maybe UserDTO -> Project
toTemporaryProject kmEditor package mCurrentUser =
  Project
    { uuid = kmEditor.uuid
    , name = kmEditor.name
    , description = Just kmEditor.description
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageUuid = fromMaybe package.uuid kmEditor.previousPackageUuid
    , selectedQuestionTagUuids = []
    , projectTags = []
    , language = Nothing
    , documentTemplateUuid = Nothing
    , formatUuid = Nothing
    , documentTemplateLanguage = Nothing
    , creatorUuid = fmap (.uuid) mCurrentUser
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = kmEditor.tenantUuid
    , createdAt = kmEditor.createdAt
    , updatedAt = kmEditor.updatedAt
    }

toDocPersistentCommand :: U.UUID -> DocumentContext -> Document -> PersistentCommand U.UUID
toDocPersistentCommand uuid docContext doc =
  toPersistentCommand
    uuid
    "doc_worker"
    "generateDocument"
    (encodeJsonToString docContext)
    10
    doc.tenantUuid
    doc.createdBy
    doc.createdAt
