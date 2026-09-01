module Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations where

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Tenant
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import qualified Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper as PM
import Shared.Util.Date

knowledgeModelMigrationDTO :: KnowledgeModelMigrationDTO
knowledgeModelMigrationDTO =
  KnowledgeModelMigrationDTO
    { editorUuid = amsterdamKnowledgeModelEditorList.uuid
    , editorName = amsterdamKnowledgeModelEditorList.name
    , editorPreviousPackage = PM.toSuggestion netherlandsKmPackage
    , state = ConflictKnowledgeModelMigrationState {targetEvent = Just . Prelude.head . fmap toEvent $ netherlandsKmPackageV2Events}
    , targetPackage = PM.toSuggestion netherlandsKmPackageV2
    , currentKnowledgeModel = Just km1Netherlands
    }

knowledgeModelMigrationCreateDTO :: KnowledgeModelMigrationCreateDTO
knowledgeModelMigrationCreateDTO = KnowledgeModelMigrationCreateDTO {targetPackageUuid = netherlandsKmPackageV2.uuid}

knowledgeModelMigrationResolutionDTO :: KnowledgeModelMigrationResolutionDTO
knowledgeModelMigrationResolutionDTO =
  KnowledgeModelMigrationResolutionDTO
    { originalEventUuid = a_km1_ch4.uuid
    , action = RejectKnowledgeModelMigrationAction
    }

differentKnowledgeModelMigration :: KnowledgeModelMigration
differentKnowledgeModelMigration =
  KnowledgeModelMigration
    { editorUuid = differentKnowledgeModelEditor.uuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , state = CompletedKnowledgeModelMigrationState
    , editorPreviousPackageUuid = differentPackage.uuid
    , targetPackageUuid = differentPackage.uuid
    , editorPreviousPackageEvents = []
    , targetPackageEvents = []
    , resultEvents = []
    , currentKnowledgeModel = Nothing
    , tenantUuid = differentTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
