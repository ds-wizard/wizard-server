module TestMigration where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query, execute_, withTransaction)

import Shared.Constant.Tenant
import Shared.Database.DAO.Common (runDB)
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.RoleDAO (insertRole)
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import qualified Shared.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified Shared.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Shared.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import qualified Shared.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplateMigration
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Shared.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration as ExternalLink
import qualified Shared.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration as KnowledgeModelCache
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelLocaleSchemaMigration as KnowledgeModelLocale
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationSchemaMigration as KnowledgeModelMigration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration as KnowledgeModelSecret
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LocaleMigration
import qualified Shared.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientSchemaMigration as OpenIdClient
import qualified Shared.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import qualified Shared.Database.Migration.Development.Plugin.PluginSchemaMigration as Plugin
import qualified Shared.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import qualified Shared.Database.Migration.Development.Project.ProjectSchemaMigration as Project
import qualified Shared.Database.Migration.Development.Registry.RegistrySchemaMigration as Registry
import qualified Shared.Database.Migration.Development.Submission.SubmissionSchemaMigration as Submission
import qualified Shared.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration as TemporaryFile
import Shared.Database.Migration.Development.Tenant.Data.TenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import qualified Shared.Database.Migration.Development.Tenant.TenantSchemaMigration as Tenant
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.RoleSchemaMigration as Role
import qualified Shared.Database.Migration.Development.User.UserOpenIdIdentitySchemaMigration as UserOpenIdIdentity
import qualified Shared.Database.Migration.Development.User.UserRegistrationPendingSchemaMigration as UserRegistrationPending
import qualified Shared.Database.Migration.Development.User.UserSchemaMigration as User
import qualified Shared.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration as UserEmailLink
import Shared.Model.Cache.ServerCache
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfig
import WizardServer.Model.Context.RequestContext

import Specs.Common

buildSchema requestContext = do
  putStrLn "DB: dropping DB triggers"
  runInContext Document.dropTriggers requestContext
  runInContext Project.dropTriggers requestContext
  runInContext Locale.dropTriggers requestContext
  runInContext KnowledgeModelLocale.dropTriggers requestContext
  putStrLn "DB: dropping DB functions"
  runInContext Project.dropFunctions requestContext
  runInContext DocumentTemplate.dropFunctions requestContext
  runInContext KnowledgeModelEditor.dropFunctions requestContext
  runInContext KnowledgeModelPackage.dropFunctions requestContext
  runInContext Common.dropFunctions requestContext
  putStrLn "DB: dropping schema"
  runInContext ExternalLink.dropTables requestContext
  runInContext Component.dropTables requestContext
  runInContext Registry.dropTables requestContext
  runInContext Audit.dropTables requestContext
  runInContext Prefab.dropTables requestContext
  runInContext PersistentCommand.dropTables requestContext
  runInContext Submission.dropTables requestContext
  runInContext UserEmailLink.dropTables requestContext
  runInContext KnowledgeModelMigration.dropTables requestContext
  runInContext KnowledgeModelCache.dropTables requestContext
  runInContext KnowledgeModelEditor.dropTables requestContext
  runInContext Document.dropTables requestContext
  runInContext Project.dropTables requestContext
  runInContext KnowledgeModelSecret.dropTables requestContext
  runInContext KnowledgeModelLocale.dropTables requestContext
  runInContext KnowledgeModelPackage.dropTables requestContext
  runInContext TemporaryFile.dropTables requestContext
  runInContext UserRegistrationPending.dropTables requestContext
  runInContext UserOpenIdIdentity.dropTables requestContext
  runInContext User.dropTables requestContext
  runInContext Role.dropTables requestContext
  runInContext Tenant.dropConfigTables requestContext
  runInContext OpenIdClient.dropTables requestContext
  runInContext DocumentTemplate.dropTables requestContext
  runInContext Locale.dropTables requestContext
  runInContext Plugin.dropTables requestContext
  runInContext Tenant.dropTables requestContext
  runInContext Instance.dropTables requestContext
  putStrLn "DB: Drop DB types"
  runInContext Common.dropTypes requestContext
  -- 2. Create
  putStrLn "DB: Create DB types"
  runInContext Common.createTypes requestContext
  putStrLn "DB: Creating schema"
  runInContext Instance.createTables requestContext
  runInContext Tenant.createTables requestContext
  runInContext Plugin.createTables requestContext
  runInContext Locale.createTables requestContext
  runInContext DocumentTemplate.createTables requestContext
  runInContext Tenant.createConfigTables requestContext
  runInContext OpenIdClient.createTables requestContext
  runInContext Role.createTables requestContext
  runInContext User.createTables requestContext
  runInContext UserOpenIdIdentity.createTables requestContext
  runInContext UserRegistrationPending.createTables requestContext
  runInContext TemporaryFile.createTables requestContext
  runInContext KnowledgeModelPackage.createTables requestContext
  runInContext KnowledgeModelLocale.createTables requestContext
  runInContext KnowledgeModelSecret.createTables requestContext
  runInContext UserEmailLink.createTables requestContext
  runInContext KnowledgeModelEditor.createTables requestContext
  runInContext KnowledgeModelCache.createTables requestContext
  runInContext Project.createTables requestContext
  runInContext DocumentTemplate.createDraftDataTable requestContext
  runInContext Document.createTables requestContext
  runInContext KnowledgeModelMigration.createTables requestContext
  runInContext Submission.createTables requestContext
  runInContext PersistentCommand.createTables requestContext
  runInContext Prefab.createTables requestContext
  runInContext Audit.createTables requestContext
  runInContext Registry.createTables requestContext
  runInContext Component.createTables requestContext
  runInContext ExternalLink.createTables requestContext
  putStrLn "DB: Creating DB functions"
  runInContext Common.createFunctions requestContext
  runInContext KnowledgeModelPackage.createFunctions requestContext
  runInContext KnowledgeModelEditor.createFunctions requestContext
  runInContext DocumentTemplate.createFunctions requestContext
  runInContext Project.createFunctions requestContext
  putStrLn "DB: Creating missing foreign key constraints"
  runInContext User.createUserLocaleForeignKeyConstraint requestContext
  putStrLn "DB: Creating triggers"
  runInContext Locale.createTriggers requestContext
  runInContext KnowledgeModelLocale.createTriggers requestContext
  runInContext Project.createTriggers requestContext
  runInContext Document.createTriggers requestContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext DocumentTemplateMigration.runS3Migration requestContext
  runInContext LocaleMigration.runS3Migration requestContext

resetDB requestContext = withTransaction (fromJust requestContext.dbConnection) (resetDBContent requestContext)

resetDBContent requestContext = do
  runInContext (runDB (`execute_` deleteAllEntitiesSql)) requestContext
  runInContext (insertTenant defaultTenant) requestContext
  runInContext (insertPlugin plugin1) requestContext
  runInContext (insertPlugin differentPlugin1) requestContext
  runInContext (insertLimitBundle defaultTenantLimitBundle) requestContext
  runInContext (insertTenant differentTenant) requestContext
  runInContext (insertLimitBundle differentTenantLimitBundle) requestContext
  runInContext (insertTenantConfigOrganization defaultOrganization) requestContext
  runInContext (insertTenantConfigAuthentication defaultAuthenticationEncrypted) requestContext
  runInContext (insertTenantConfigPrivacyAndSupport defaultPrivacyAndSupport) requestContext
  runInContext (insertTenantConfigDashboardAndLoginScreen defaultDashboardAndLoginScreen) requestContext
  runInContext (insertTenantConfigDashboardAndLoginScreenAnnouncement defaultDashboardAndLoginScreenAnnouncement) requestContext
  runInContext (insertTenantConfigLookAndFeel defaultLookAndFeel) requestContext
  runInContext (insertTenantConfigLookAndFeelCustomMenuLink defaultLookAndFeelCustomLink) requestContext
  runInContext (insertTenantConfigRegistry defaultRegistryEncrypted) requestContext
  runInContext (insertTenantConfigProject defaultProjectEncrypted) requestContext
  runInContext (insertTenantConfigSubmission (defaultSubmission {services = []})) requestContext
  runInContext (insertTenantConfigFeatures defaultFeatures) requestContext
  runInContext (insertTenantConfigMail defaultMail) requestContext
  runInContext (insertTenantConfigOwl defaultOwl) requestContext
  runInContext (insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = differentTenantUuid})) requestContext
  runInContext (insertRole adminRole) requestContext
  runInContext (insertRole dataStewardRole) requestContext
  runInContext (insertRole researcherRole) requestContext
  runInContext (insertRole differentAdminRole) requestContext
  runInContext (insertRole differentDataStewardRole) requestContext
  runInContext (insertRole differentResearcherRole) requestContext
  runInContext (insertUser userSystem) requestContext
  runInContext (insertUser userAlbert) requestContext
  runInContext (insertUserToken albertToken) requestContext
  runInContext (insertUser userCharles) requestContext
  runInContext (insertPackage globalKmPackageEmpty) requestContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEmptyEvents) requestContext
  runInContext (insertPackage globalKmPackage) requestContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEvents) requestContext
  runInContext (insertPackage netherlandsKmPackage) requestContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageEvents) requestContext
  runInContext (insertPackage netherlandsKmPackageV2) requestContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageV2Events) requestContext
  runInContext (insertPackage differentPackage) requestContext
  runInContext (traverse_ insertPackageEvent differentPackageEvents) requestContext
  return ()

deleteAllEntitiesSql :: Query
deleteAllEntitiesSql =
  fromString . intercalate "; " . fmap ("DELETE FROM " <>) $
    [ "external_link_usage"
    , "registry_organization"
    , "registry_knowledge_model_package"
    , "registry_document_template"
    , "audit"
    , "prefab"
    , "persistent_command"
    , "submission"
    , "config_owl"
    , "config_mail"
    , "config_features"
    , "config_submission"
    , "config_project"
    , "config_registry"
    , "config_look_and_feel_custom_menu_link"
    , "config_look_and_feel"
    , "config_dashboard_and_login_screen_announcement"
    , "config_dashboard_and_login_screen"
    , "config_privacy_and_support"
    , "config_authentication"
    , "config_organization"
    , "knowledge_model_migration"
    , "user_email_link"
    , "knowledge_model_editor"
    , "document"
    , "document_template_draft_data"
    , "project_version"
    , "project_event"
    , "project_file"
    , "project_comment"
    , "project_comment_thread"
    , "project_perm_user"
    , "project_perm_group"
    , "project"
    , "document_template_file"
    , "document_template_asset"
    , "document_template"
    , "knowledge_model_secret"
    , "knowledge_model_locale"
    , "knowledge_model_package"
    , "user_token"
    , "user_group_membership"
    , "user_openid_identity"
    , "user_tour"
    , "user_entity"
    , "role"
    , "user_group"
    , "locale"
    , "tenant_limit_bundle"
    , "plugin"
    , "tenant"
    , "component"
    ]
