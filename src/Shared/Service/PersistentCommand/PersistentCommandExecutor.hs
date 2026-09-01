module Shared.Service.PersistentCommand.PersistentCommandExecutor where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import qualified Shared.Service.Document.DocumentCommandExecutor as DocumentCommandExecutor
import qualified Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetCommandExecutor as DocumentTemplateAssetCommandExecutor
import qualified Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleCommandExecutor as DocumentTemplateLocaleCommandExecutor
import qualified Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleCommandExecutor as KnowledgeModelLocaleCommandExecutor
import qualified Shared.Service.KnowledgeModel.Metamodel.MigrationCommandExecutor as MetamodelMigratorCommandExecutor
import qualified Shared.Service.Locale.LocaleCommandExecutor as LocaleCommandExecutor
import qualified Shared.Service.Prefab.PrefabCommandExecutor as PrefabCommandExecutor
import qualified Shared.Service.Project.Cache.ProjectCacheCommandExecutor as ProjectCacheCommandExecutor
import qualified Shared.Service.Project.File.ProjectFileCommandExecutor as ProjectFileCommandExecutor
import qualified Shared.Service.Project.ProjectCommandExecutor as ProjectCommandExecutor
import qualified Shared.Service.Tenant.TenantCommandExecutor as TenantCommandExecutor
import qualified Shared.Service.User.GroupMembership.UserGroupMembershipCommandExecutor as UserGroupMembershipCommandExecutor

components :: [String]
components =
  [ DocumentCommandExecutor.cComponent
  , DocumentTemplateAssetCommandExecutor.cComponent
  , DocumentTemplateLocaleCommandExecutor.cComponent
  , KnowledgeModelLocaleCommandExecutor.cComponent
  , LocaleCommandExecutor.cComponent
  , MetamodelMigratorCommandExecutor.cComponent
  , PrefabCommandExecutor.cComponent
  , ProjectCacheCommandExecutor.cComponent
  , ProjectCommandExecutor.cComponent
  , ProjectFileCommandExecutor.cComponent
  , TenantCommandExecutor.cComponent
  , UserGroupMembershipCommandExecutor.cComponent
  ]

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.component == DocumentCommandExecutor.cComponent = DocumentCommandExecutor.execute command
  | command.component == DocumentTemplateAssetCommandExecutor.cComponent = DocumentTemplateAssetCommandExecutor.execute command
  | command.component == DocumentTemplateLocaleCommandExecutor.cComponent = DocumentTemplateLocaleCommandExecutor.execute command
  | command.component == KnowledgeModelLocaleCommandExecutor.cComponent = KnowledgeModelLocaleCommandExecutor.execute command
  | command.component == LocaleCommandExecutor.cComponent = LocaleCommandExecutor.execute command
  | command.component == MetamodelMigratorCommandExecutor.cComponent = MetamodelMigratorCommandExecutor.execute command
  | command.component == PrefabCommandExecutor.cComponent = PrefabCommandExecutor.execute command
  | command.component == ProjectCacheCommandExecutor.cComponent = ProjectCacheCommandExecutor.execute command
  | command.component == ProjectCommandExecutor.cComponent = ProjectCommandExecutor.execute command
  | command.component == ProjectFileCommandExecutor.cComponent = ProjectFileCommandExecutor.execute command
  | command.component == TenantCommandExecutor.cComponent = TenantCommandExecutor.execute command
  | command.component == UserGroupMembershipCommandExecutor.cComponent = UserGroupMembershipCommandExecutor.execute command
  | otherwise = throwError . GeneralServerError $ "Unknown command component: " <> command.component
