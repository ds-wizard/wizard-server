module Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors where

import Data.Either (rights)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Shared.Constant.KnowledgeModel
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.ProjectReply
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.KnowledgeModel.Compiler.Compiler
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import qualified Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as SPM
import qualified Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper as PM
import Shared.Util.Uuid

amsterdamKnowledgeModelEditorList :: KnowledgeModelEditorList
amsterdamKnowledgeModelEditorList =
  KnowledgeModelEditorList
    { uuid = u' "6474b24b-262b-42b1-9451-008e8363f2b6"
    , name = amsterdamKmPackage.name
    , id = amsterdamKmPackage.id
    , version = amsterdamKmPackage.version
    , previousPackageUuid = Just netherlandsKmPackage.uuid
    , forkOfPackage = Just $ PM.toSuggestion netherlandsKmPackage
    , state = EditedKnowledgeModelEditorState
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , workspaceUuid = defaultWorkspaceUuid
    }

amsterdamKnowledgeModelEditor :: KnowledgeModelEditor
amsterdamKnowledgeModelEditor =
  KnowledgeModelEditor
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    , id = amsterdamKnowledgeModelEditorList.id
    , version = "1.0.0"
    , description = "First Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , language = "en"
    , previousPackageUuid = amsterdamKnowledgeModelEditorList.previousPackageUuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = amsterdamKnowledgeModelEditorList.createdBy
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = defaultWorkspaceUuid
    , createdAt = amsterdamKnowledgeModelEditorList.createdAt
    , updatedAt = amsterdamKnowledgeModelEditorList.updatedAt
    }

amsterdamKnowledgeModelEditorSuggestion :: KnowledgeModelEditorSuggestion
amsterdamKnowledgeModelEditorSuggestion =
  KnowledgeModelEditorSuggestion
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    }

amsterdamKnowledgeModelEditorEvents :: [KnowledgeModelEditorEvent]
amsterdamKnowledgeModelEditorEvents = fmap (toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid) amsterdamEvents

amsterdamEvents :: [KnowledgeModelEvent]
amsterdamEvents =
  [ a_km1_ir
  , a_km1_ch1_q1
  , a_km1_ch1_q2
  , a_km1_ch1_q2_aNo1
  , a_km1_ch1_q2_aYes1
  , a_km1_ch1_ansYes1_fuq1
  , a_km1_ch1_q2_aYes1_fuq1_aNo
  , a_km1_ch1_q2_aYesFu1
  , a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
  , a_km1_ch1_q2_aNoFu2
  , a_km1_ch1_q2_aYesFu2
  , a_km1_ch1_q2_eAlbert
  , a_km1_ch1_q2_eNikola
  , a_km1_ch1_q2_rCh1
  , a_km1_ch1_q2_rCh2
  , a_km1_ch2
  , a_km1_ch2_q3
  , a_km1_ch2_q3_aNo2
  , a_km1_ch2_q3_aYes2
  , a_km1_ch3
  , a_km1_ch3_q15
  ]

amsterdamKnowledgeModelEditorReplies :: M.Map String Reply
amsterdamKnowledgeModelEditorReplies = M.empty

amsterdamKnowledgeModelEditorCreate :: KnowledgeModelEditorCreateDTO
amsterdamKnowledgeModelEditorCreate =
  KnowledgeModelEditorCreateDTO
    { name = amsterdamKnowledgeModelEditorList.name
    , id = amsterdamKnowledgeModelEditorList.id
    , version = "1.0.0"
    , language = Nothing
    , previousPackageUuid = amsterdamKnowledgeModelEditorList.previousPackageUuid
    }

amsterdamKnowledgeModelEditorChange :: KnowledgeModelEditorChangeDTO
amsterdamKnowledgeModelEditorChange =
  KnowledgeModelEditorChangeDTO
    { name = "EDITED: " ++ amsterdamKnowledgeModelEditorList.name
    , id = amsterdamKnowledgeModelEditorList.id
    , version = "2.0.0"
    , description = "EDITED: description"
    , readme = "EDITED: Readme"
    , license = "Apache-3.0"
    , language = "de"
    }

amsterdamKnowledgeModelEditorKnowledgeModel :: KnowledgeModel
amsterdamKnowledgeModelEditorKnowledgeModel =
  head . rights $ [compile Nothing . fmap SPM.toEvent $ globalKmPackageEvents ++ netherlandsKmPackageEvents]

amsterdamKnowledgeModelEditorDetail :: KnowledgeModelEditorDetailDTO
amsterdamKnowledgeModelEditorDetail =
  KnowledgeModelEditorDetailDTO
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    , id = amsterdamKnowledgeModelEditorList.id
    , version = amsterdamKnowledgeModelEditor.version
    , description = amsterdamKnowledgeModelEditor.description
    , readme = amsterdamKnowledgeModelEditor.readme
    , license = amsterdamKnowledgeModelEditor.license
    , language = amsterdamKnowledgeModelEditor.language
    , state = EditedKnowledgeModelEditorState
    , previousPackage = Just . SPM.toSimple $ netherlandsKmPackage
    , forkOfPackage = Just . PM.toSimpleDTO $ netherlandsKmPackage
    , createdBy = amsterdamKnowledgeModelEditorList.createdBy
    , events = amsterdamEvents
    , replies = amsterdamKnowledgeModelEditorReplies
    , knowledgeModel = amsterdamKnowledgeModelEditorKnowledgeModel
    , createdAt = amsterdamKnowledgeModelEditorList.createdAt
    , updatedAt = amsterdamKnowledgeModelEditorList.updatedAt
    , workspaceUuid = defaultWorkspaceUuid
    }

leidenKnowledgeModelEditor :: KnowledgeModelEditorList
leidenKnowledgeModelEditor =
  KnowledgeModelEditorList
    { uuid = u' "47421955-ba30-48d4-8c49-9ec47eda2cad"
    , name = "Leiden KM"
    , id = "org.nl.amsterdam.leiden-km"
    , version = "1.0.0"
    , state = DefaultKnowledgeModelEditorState
    , previousPackageUuid = Just netherlandsKmPackage.uuid
    , forkOfPackage = Just $ PM.toSuggestion netherlandsKmPackage
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , workspaceUuid = defaultWorkspaceUuid
    }

leidenKnowledgeModelEditorCreate :: KnowledgeModelEditorCreateDTO
leidenKnowledgeModelEditorCreate =
  KnowledgeModelEditorCreateDTO
    { name = leidenKnowledgeModelEditor.name
    , id = leidenKnowledgeModelEditor.id
    , version = "1.0.0"
    , language = Nothing
    , previousPackageUuid = leidenKnowledgeModelEditor.previousPackageUuid
    }

differentKnowledgeModelEditor :: KnowledgeModelEditor
differentKnowledgeModelEditor =
  KnowledgeModelEditor
    { uuid = u' "fc49b6a5-51ae-4442-82e8-c3bf216545ec"
    , name = "KnowledgeModelEditor Events"
    , id = "org.nl.amsterdam.my-km"
    , version = "1.0.0"
    , description = "Some desc"
    , readme = "Some readme"
    , license = "Apache-2.0"
    , language = "en"
    , previousPackageUuid = Just differentPackage.uuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = Just userCharles.uuid
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = differentWorkspaceUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentKnowledgeModelEditorReplies :: M.Map String Reply
differentKnowledgeModelEditorReplies = M.empty

packagePublishEditorDTO :: PackagePublishEditorDTO
packagePublishEditorDTO =
  PackagePublishEditorDTO
    { editorUuid = amsterdamKnowledgeModelEditor.uuid
    , localeUuids = Nothing
    }

packagePublishMigrationDTO :: PackagePublishMigrationDTO
packagePublishMigrationDTO =
  PackagePublishMigrationDTO
    { editorUuid = amsterdamKnowledgeModelEditor.uuid
    , version = amsterdamKmPackage.version
    , description = amsterdamKmPackage.description
    , readme = amsterdamKmPackage.readme
    , localeUuids = Nothing
    }

secondWorkspaceKnowledgeModelEditor :: KnowledgeModelEditor
secondWorkspaceKnowledgeModelEditor =
  amsterdamKnowledgeModelEditor
    { uuid = u' "b7e2d4c6-1a3f-4e5b-9c8d-2f6a7b8c9d0e"
    , name = "Second Workspace Knowledge Model"
    , id = "org.nl.amsterdam.core-second-workspace"
    , workspaceUuid = secondWorkspace.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    }

secondWorkspaceKnowledgeModelEditorList :: KnowledgeModelEditorList
secondWorkspaceKnowledgeModelEditorList =
  amsterdamKnowledgeModelEditorList
    { uuid = secondWorkspaceKnowledgeModelEditor.uuid
    , name = secondWorkspaceKnowledgeModelEditor.name
    , id = secondWorkspaceKnowledgeModelEditor.id
    , state = DefaultKnowledgeModelEditorState
    , createdAt = secondWorkspaceKnowledgeModelEditor.createdAt
    , updatedAt = secondWorkspaceKnowledgeModelEditor.updatedAt
    , workspaceUuid = secondWorkspace.uuid
    }
