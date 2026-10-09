module Shared.Database.Migration.Development.Document.Data.Documents where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import qualified Shared.Constant.DocumentTemplate as TemplateConstant
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Report.Data.Reports
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.Document.Context.DocumentContextHash
import Shared.Service.Document.Context.DocumentContextMapper
import Shared.Service.Document.DocumentMapper
import Shared.Util.Uuid

doc1 :: Document
doc1 =
  Document
    { uuid = u' "264ca352-1a99-4ffd-860e-32aee9a98428"
    , name = "My exported document"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just project1.uuid
    , projectEventUuid = Just . getUuid . last $ project1Events
    , documentTemplateUuid = wizardDocumentTemplate.uuid
    , formatUuid = formatJson.uuid
    , language = Nothing
    , createdBy = Just userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = defaultWorkspaceUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , contextHash = Just dmp1.hash
    , knowledgeModelEditorUuid = Nothing
    }

doc1Content :: BS.ByteString
doc1Content =
  BS.pack $
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod "
      ++ "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet "
      ++ "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla "
      ++ "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend "
      ++ "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. "
      ++ "Aliquam volutpat molestie nibh sit amet iaculis."

dmp1 :: DocumentContext
dmp1 = dmp1WithoutHash {hash = computeContextHash dmp1WithoutHash}

dmp1WithoutHash :: DocumentContext
dmp1WithoutHash =
  DocumentContext
    { metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , hash = ""
    , workspace = toDocumentContextWorkspace defaultWorkspace
    , project = Just dmp1Project
    , knowledgeModelEditor = Nothing
    , questionnaire = dmp1Questionnaire
    , knowledgeModel = km1WithQ4
    , knowledgeModelPackage = Just dmp1Package
    , document = Nothing
    , config = dmpConfig
    }

dmp1Project :: DocumentContextProject
dmp1Project = toDocumentContextProject project1 (Just userAlbert) [dmp1AlbertPerm] []

dmp1AlbertPerm :: DocumentContextUserPerm
dmp1AlbertPerm = DocumentContextUserPerm {user = toDocumentContextUser userAlbert, perms = L.sort ownerPermissions}

dmp1Questionnaire :: DocumentContextQuestionnaire
dmp1Questionnaire =
  DocumentContextQuestionnaire
    { replies = project1Ctn.replies
    , labels = project1Ctn.labels
    , phaseUuid = project1Ctn.phaseUuid
    , versionUuid = Just (projectVersion1 project1.uuid).uuid
    , versions = qVersionsList project1.uuid
    , files = []
    , report = toDocumentContextReport report1
    , language = project1.language
    }

dmp1Package :: DocumentContextPackage
dmp1Package = toDocumentContextPackage germanyKmPackage

dmp1Document :: DocumentContextDocument
dmp1Document = toDocumentContextDocument doc1 wizardDocumentTemplate formatJsonSimple (Just userNikola) Nothing

dmpKmEditor1 :: DocumentContext
dmpKmEditor1 = dmpKmEditor1WithoutHash {hash = computeContextHash dmpKmEditor1WithoutHash}

dmpKmEditor1WithoutHash :: DocumentContext
dmpKmEditor1WithoutHash =
  DocumentContext
    { metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , hash = ""
    , workspace = toDocumentContextWorkspace defaultWorkspace
    , project = Nothing
    , knowledgeModelEditor = Just dmpKmEditor1Editor
    , questionnaire =
        DocumentContextQuestionnaire
          { replies = amsterdamKnowledgeModelEditorReplies
          , labels = M.empty
          , phaseUuid = Nothing
          , versionUuid = Nothing
          , versions = []
          , files = []
          , report = toDocumentContextReport report1
          , language = Just amsterdamKnowledgeModelEditor.language
          }
    , knowledgeModel = amsterdamKnowledgeModelEditorKnowledgeModel
    , knowledgeModelPackage = Just $ toDocumentContextPackage netherlandsKmPackage
    , document = Nothing
    , config = dmpConfig
    }

dmpKmEditor1Editor :: DocumentContextKnowledgeModelEditor
dmpKmEditor1Editor = toDocumentContextKnowledgeModelEditor amsterdamKnowledgeModelEditor (Just netherlandsKmPackage) (Just userAlbert)

dmpConfig :: DocumentContextConfig
dmpConfig =
  DocumentContextConfig
    { clientUrl = "https://example.com"
    , appTitle = Nothing
    , primaryColor = Nothing
    , logoUrl = Nothing
    }

doc1Create :: DocumentCreateDTO
doc1Create =
  DocumentCreateDTO
    { name = doc1.name
    , projectUuid = project1.uuid
    , projectEventUuid = Just . getUuid . last $ project1Events
    , documentTemplateUuid = doc1.documentTemplateUuid
    , formatUuid = doc1.formatUuid
    , language = Nothing
    }

doc1Dto :: DocumentDTO
doc1Dto = toDTOWithDocTemplate doc1 project1 Nothing [] wizardDocumentTemplate formatJsonSimple

doc2 :: Document
doc2 =
  Document
    { uuid = u' "12de4935-58ad-4a34-9d91-dd0e16619b35"
    , name = "My exported document 2"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just project2.uuid
    , projectEventUuid = Just . getUuid . last $ project2Events
    , documentTemplateUuid = wizardDocumentTemplate.uuid
    , formatUuid = formatJson.uuid
    , language = Nothing
    , createdBy = Just userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = defaultWorkspaceUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , contextHash = Nothing
    , knowledgeModelEditorUuid = Nothing
    }

doc3 :: Document
doc3 =
  Document
    { uuid = u' "35ef63fd-cb5c-448c-9a4f-54b572573c20"
    , name = "My exported document 3"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just project2.uuid
    , projectEventUuid = Just . getUuid . last $ project2Events
    , documentTemplateUuid = wizardDocumentTemplate.uuid
    , formatUuid = formatJson.uuid
    , language = Nothing
    , createdBy = Just userAlbert.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = defaultWorkspaceUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , contextHash = Nothing
    , knowledgeModelEditorUuid = Nothing
    }

doc4 :: Document
doc4 =
  doc1
    { uuid = u' "9e010fc5-d147-4e9a-94a0-5aba40d78b97"
    , name = "My failed document 4"
    , state = ErrorDocumentState
    }

doc5 :: Document
doc5 =
  doc1
    { uuid = u' "c3e1a760-0941-499c-a8cd-6b9d78eee0ba"
    , name = "My in progress document 5"
    , state = InProgressDocumentState
    }

doc6 :: Document
doc6 =
  doc1
    { uuid = u' "6a7631bc-af69-4e72-83e4-8440be071005"
    , name = "My queued document 6"
    , state = QueuedDocumentState
    }

tempDocQueued :: Document
tempDocQueued =
  doc1
    { uuid = u' "537e2b86-64ec-4ee9-965b-6637775f8f89"
    , name = "My temp docs"
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocInProgress :: Document
tempDocInProgress =
  doc1
    { uuid = u' "8f075d1e-d7ca-416e-8e17-5dd6d53a01f3"
    , name = "My temp docs"
    , state = InProgressDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocDone :: Document
tempDocDone =
  doc1
    { uuid = u' "ac38c865-a891-43a7-986b-b6801ed10880"
    , name = "My temp docs"
    , state = DoneDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocError :: Document
tempDocError =
  doc1
    { uuid = u' "16884341-2771-437d-944f-69bc9572af20"
    , name = "My temp docs"
    , state = ErrorDocumentState
    , durability = TemporallyDocumentDurability
    }

differentDoc :: Document
differentDoc =
  Document
    { uuid = u' "b9a72acc-8261-4c38-b2d5-bdefea241d59"
    , name = "My different document"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just differentProject.uuid
    , projectEventUuid = Nothing
    , documentTemplateUuid = differentDocumentTemplate.uuid
    , formatUuid = formatJson.uuid
    , language = Nothing
    , createdBy = Just userCharles.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = differentWorkspaceUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , contextHash = Nothing
    , knowledgeModelEditorUuid = Nothing
    }

secondWorkspaceDoc :: Document
secondWorkspaceDoc =
  doc1
    { uuid = u' "0d3e9b7a-5c21-4f8e-a6b4-7c1d2e3f4a5b"
    , name = "Second workspace document"
    , projectUuid = Just project16.uuid
    , projectEventUuid = Nothing
    , workspaceUuid = project16.workspaceUuid
    , contextHash = Nothing
    }
