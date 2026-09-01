module Shared.Database.Migration.Development.Migration (
  runMigration,
) where

import Shared.Constant.Component
import qualified Shared.Database.Migration.Development.Audit.AuditMigration as Audit
import qualified Shared.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified Shared.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Shared.Database.Migration.Development.Component.ComponentMigration as Component
import qualified Shared.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as Document
import qualified Shared.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Shared.Database.Migration.Development.ExternalLink.ExternalLinkMigration as ExternalLink
import qualified Shared.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration as ExternalLink
import qualified Shared.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration as KnowledgeModelCache
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelLocaleSchemaMigration as KnowledgeModelLocale
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration as KnowledgeModelMigration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationSchemaMigration as KnowledgeModelMigration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration as KnowledgeModelSecret
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration as KnowledgeModelSecret
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified Shared.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OpenIdClient
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientSchemaMigration as OpenIdClient
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientSessionSchemaMigration as OpenIdClientSession
import qualified Shared.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PersistentCommand
import qualified Shared.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified Shared.Database.Migration.Development.Plugin.PluginMigration as Plugin
import qualified Shared.Database.Migration.Development.Plugin.PluginSchemaMigration as Plugin
import qualified Shared.Database.Migration.Development.Prefab.PrefabMigration as Prefab
import qualified Shared.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as Project
import qualified Shared.Database.Migration.Development.Project.ProjectSchemaMigration as Project
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry
import qualified Shared.Database.Migration.Development.Registry.RegistrySchemaMigration as Registry
import qualified Shared.Database.Migration.Development.Submission.SubmissionSchemaMigration as Submission
import qualified Shared.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration as TemporaryFile
import qualified Shared.Database.Migration.Development.Tenant.TenantMigration as Tenant
import qualified Shared.Database.Migration.Development.Tenant.TenantSchemaMigration as Tenant
import qualified Shared.Database.Migration.Development.User.RoleMigration as Role
import qualified Shared.Database.Migration.Development.User.RoleSchemaMigration as Role
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import qualified Shared.Database.Migration.Development.User.UserOpenIdIdentitySchemaMigration as UserOpenIdIdentity
import qualified Shared.Database.Migration.Development.User.UserRegistrationPendingSchemaMigration as UserRegistrationPending
import qualified Shared.Database.Migration.Development.User.UserSchemaMigration as User
import qualified Shared.Database.Migration.Development.UserEmailLink.UserEmailLinkMigration as UserEmailLink
import qualified Shared.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration as UserEmailLink
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m (Maybe String)
runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB triggers
  Document.dropTriggers
  DocumentTemplate.dropTriggers
  Project.dropTriggers
  Locale.dropTriggers
  KnowledgeModelLocale.dropTriggers
  -- 2. Drop DB functions
  Project.dropFunctions
  DocumentTemplate.dropFunctions
  KnowledgeModelEditor.dropFunctions
  KnowledgeModelPackage.dropFunctions
  Common.dropFunctions
  -- 3. Drop DB schema
  ExternalLink.dropTables
  Component.dropTables
  Registry.dropTables
  Audit.dropTables
  Prefab.dropTables
  PersistentCommand.dropTables
  Submission.dropTables
  UserEmailLink.dropTables
  KnowledgeModelMigration.dropTables
  KnowledgeModelEditor.dropTables
  KnowledgeModelCache.dropTables
  Document.dropTables
  Project.dropTables
  KnowledgeModelSecret.dropTables
  KnowledgeModelLocale.dropTables
  KnowledgeModelPackage.dropTables
  TemporaryFile.dropTables
  UserRegistrationPending.dropTables
  UserOpenIdIdentity.dropTables
  User.dropTables
  Role.dropTables
  Tenant.dropConfigTables
  OpenIdClientSession.dropTables
  OpenIdClient.dropTables
  DocumentTemplate.dropTables
  Locale.dropTables
  Plugin.dropTables
  Tenant.dropTables
  Instance.dropTables
  -- 4. Drop DB Types
  Common.dropTypes
  -- 5. Create DB Types
  Common.createTypes
  -- 6. Create schema
  Instance.createTables
  Tenant.createTables
  Plugin.createTables
  Locale.createTables
  DocumentTemplate.createTables
  Tenant.createConfigTables
  OpenIdClientSession.createTables
  OpenIdClient.createTables
  Role.createTables
  User.createTables
  UserOpenIdIdentity.createTables
  UserRegistrationPending.createTables
  TemporaryFile.createTables
  KnowledgeModelPackage.createTables
  KnowledgeModelLocale.createTables
  KnowledgeModelSecret.createTables
  UserEmailLink.createTables
  KnowledgeModelEditor.createTables
  KnowledgeModelCache.createTables
  Project.createTables
  DocumentTemplate.createDraftDataTable
  Document.createTables
  KnowledgeModelMigration.createTables
  Submission.createTables
  PersistentCommand.createTables
  Prefab.createTables
  Audit.createTables
  Registry.createTables
  Component.createTables
  ExternalLink.createTables
  -- 7. Create DB functions
  Common.createFunctions
  KnowledgeModelPackage.createFunctions
  KnowledgeModelEditor.createFunctions
  DocumentTemplate.createFunctions
  Project.createFunctions
  -- 8. Create missing foreign key constraints
  User.createUserLocaleForeignKeyConstraint
  -- 9. Create DB triggers
  Locale.createTriggers
  KnowledgeModelLocale.createTriggers
  Project.createTriggers
  Document.createTriggers
  DocumentTemplate.createTriggers
  -- 10. Load S3 fixtures
  DocumentTemplate.runS3Migration
  Locale.runS3Migration
  -- 11. Load fixtures
  Tenant.runMigration
  OpenIdClient.runMigration
  Plugin.runMigration
  Role.runMigration
  User.runMigration
  KnowledgeModelPackage.runMigration
  KnowledgeModelSecret.runMigration
  DocumentTemplate.runMigration
  UserEmailLink.runMigration
  KnowledgeModelEditor.runMigration
  Project.runMigration
  Document.runMigration
  KnowledgeModelMigration.runMigration
  PersistentCommand.runMigration
  Prefab.runMigration
  Audit.runMigration
  Registry.runMigration
  Locale.runMigration
  Component.runMigration
  ExternalLink.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
