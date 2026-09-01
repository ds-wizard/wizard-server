module Wizard.TestMigration where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query, execute_, withTransaction)

import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import Shared.Common.Constant.Tenant
import Shared.Common.Database.DAO.Common (runDB)
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import Wizard.Database.DAO.Plugin.PluginDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplateMigration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as Feedback
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration as KnowledgeModelCache
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelLocaleSchemaMigration as KnowledgeModelLocale
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationSchemaMigration as KnowledgeModelMigration
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration as KnowledgeModelSecret
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LocaleMigration
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import qualified Wizard.Database.Migration.Development.Plugin.PluginSchemaMigration as Plugin
import qualified Wizard.Database.Migration.Development.Project.ProjectSchemaMigration as Project
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as Registry
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as Submission
import qualified Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration as TemporaryFile
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import qualified Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration as Tenant
import Wizard.Database.Migration.Development.User.Data.Roles
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.RoleSchemaMigration as Role
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as User
import qualified Wizard.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration as UserEmailLink
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO
import WizardLib.Public.Database.DAO.User.RoleDAO (insertRole)
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import qualified WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration as ExternalLink
import qualified WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientSchemaMigration as OpenIdClient
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs
import qualified WizardLib.Public.Database.Migration.Development.User.UserOpenIdIdentitySchemaMigration as UserOpenIdIdentity
import qualified WizardLib.Public.Database.Migration.Development.User.UserRegistrationPendingSchemaMigration as UserRegistrationPending
import WizardLib.Public.Model.Tenant.Config.TenantConfig

import Wizard.Specs.Common

buildSchema appContext = do
  putStrLn "DB: dropping DB triggers"
  runInContext Document.dropTriggers appContext
  runInContext Project.dropTriggers appContext
  runInContext Locale.dropTriggers appContext
  runInContext KnowledgeModelLocale.dropTriggers appContext
  putStrLn "DB: dropping DB functions"
  runInContext Project.dropFunctions appContext
  runInContext DocumentTemplate.dropFunctions appContext
  runInContext KnowledgeModelEditor.dropFunctions appContext
  runInContext KnowledgeModelPackage.dropFunctions appContext
  runInContext Common.dropFunctions appContext
  putStrLn "DB: dropping schema"
  runInContext ExternalLink.dropTables appContext
  runInContext Component.dropTables appContext
  runInContext Registry.dropTables appContext
  runInContext Audit.dropTables appContext
  runInContext Prefab.dropTables appContext
  runInContext PersistentCommand.dropTables appContext
  runInContext Submission.dropTables appContext
  runInContext UserEmailLink.dropTables appContext
  runInContext Feedback.dropTables appContext
  runInContext KnowledgeModelMigration.dropTables appContext
  runInContext KnowledgeModelCache.dropTables appContext
  runInContext KnowledgeModelEditor.dropTables appContext
  runInContext Document.dropTables appContext
  runInContext Project.dropTables appContext
  runInContext KnowledgeModelSecret.dropTables appContext
  runInContext KnowledgeModelLocale.dropTables appContext
  runInContext KnowledgeModelPackage.dropTables appContext
  runInContext TemporaryFile.dropTables appContext
  runInContext UserRegistrationPending.dropTables appContext
  runInContext UserOpenIdIdentity.dropTables appContext
  runInContext User.dropTables appContext
  runInContext Role.dropTables appContext
  runInContext Tenant.dropConfigTables appContext
  runInContext OpenIdClient.dropTables appContext
  runInContext DocumentTemplate.dropTables appContext
  runInContext Locale.dropTables appContext
  runInContext Plugin.dropTables appContext
  runInContext Tenant.dropTables appContext
  runInContext Instance.dropTables appContext
  putStrLn "DB: Drop DB types"
  runInContext Common.dropTypes appContext
  -- 2. Create
  putStrLn "DB: Create DB types"
  runInContext Common.createTypes appContext
  putStrLn "DB: Creating schema"
  runInContext Instance.createTables appContext
  runInContext Tenant.createTables appContext
  runInContext Plugin.createTables appContext
  runInContext Locale.createTables appContext
  runInContext DocumentTemplate.createTables appContext
  runInContext Tenant.createConfigTables appContext
  runInContext OpenIdClient.createTables appContext
  runInContext Role.createTables appContext
  runInContext User.createTables appContext
  runInContext UserOpenIdIdentity.createTables appContext
  runInContext UserRegistrationPending.createTables appContext
  runInContext TemporaryFile.createTables appContext
  runInContext KnowledgeModelPackage.createTables appContext
  runInContext KnowledgeModelLocale.createTables appContext
  runInContext KnowledgeModelSecret.createTables appContext
  runInContext UserEmailLink.createTables appContext
  runInContext Feedback.createTables appContext
  runInContext KnowledgeModelEditor.createTables appContext
  runInContext KnowledgeModelCache.createTables appContext
  runInContext Project.createTables appContext
  runInContext DocumentTemplate.createDraftDataTable appContext
  runInContext Document.createTables appContext
  runInContext KnowledgeModelMigration.createTables appContext
  runInContext Submission.createTables appContext
  runInContext PersistentCommand.createTables appContext
  runInContext Prefab.createTables appContext
  runInContext Audit.createTables appContext
  runInContext Registry.createTables appContext
  runInContext Component.createTables appContext
  runInContext ExternalLink.createTables appContext
  putStrLn "DB: Creating DB functions"
  runInContext Common.createFunctions appContext
  runInContext KnowledgeModelPackage.createFunctions appContext
  runInContext KnowledgeModelEditor.createFunctions appContext
  runInContext DocumentTemplate.createFunctions appContext
  runInContext Project.createFunctions appContext
  putStrLn "DB: Creating missing foreign key constraints"
  runInContext User.createUserLocaleForeignKeyConstraint appContext
  putStrLn "DB: Creating triggers"
  runInContext Locale.createTriggers appContext
  runInContext KnowledgeModelLocale.createTriggers appContext
  runInContext Project.createTriggers appContext
  runInContext Document.createTriggers appContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext DocumentTemplateMigration.runS3Migration appContext
  runInContext LocaleMigration.runS3Migration appContext

resetDB appContext = withTransaction (fromJust appContext.dbConnection) (resetDBContent appContext)

resetDBContent appContext = do
  runInContext (runDB (`execute_` deleteAllEntitiesSql)) appContext
  runInContext (insertTenant defaultTenant) appContext
  runInContext (insertPlugin plugin1) appContext
  runInContext (insertPlugin differentPlugin1) appContext
  runInContext (insertLimitBundle defaultTenantLimitBundle) appContext
  runInContext (insertTenant differentTenant) appContext
  runInContext (insertLimitBundle differentTenantLimitBundle) appContext
  runInContext (insertTenantConfigOrganization defaultOrganization) appContext
  runInContext (insertTenantConfigAuthentication defaultAuthenticationEncrypted) appContext
  runInContext (insertTenantConfigPrivacyAndSupport defaultPrivacyAndSupport) appContext
  runInContext (insertTenantConfigDashboardAndLoginScreen defaultDashboardAndLoginScreen) appContext
  runInContext (insertTenantConfigDashboardAndLoginScreenAnnouncement defaultDashboardAndLoginScreenAnnouncement) appContext
  runInContext (insertTenantConfigLookAndFeel defaultLookAndFeel) appContext
  runInContext (insertTenantConfigLookAndFeelCustomMenuLink defaultLookAndFeelCustomLink) appContext
  runInContext (insertTenantConfigRegistry defaultRegistryEncrypted) appContext
  runInContext (insertTenantConfigProject defaultProjectEncrypted) appContext
  runInContext (insertTenantConfigSubmission (defaultSubmission {services = []})) appContext
  runInContext (insertTenantConfigFeatures defaultFeatures) appContext
  runInContext (insertTenantConfigMail defaultMail) appContext
  runInContext (insertTenantConfigOwl defaultOwl) appContext
  runInContext (insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = differentTenantUuid})) appContext
  runInContext (insertRole adminRole) appContext
  runInContext (insertRole dataStewardRole) appContext
  runInContext (insertRole researcherRole) appContext
  runInContext (insertRole differentAdminRole) appContext
  runInContext (insertRole differentDataStewardRole) appContext
  runInContext (insertRole differentResearcherRole) appContext
  runInContext (insertUser userSystem) appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (insertUserToken albertToken) appContext
  runInContext (insertUser userCharles) appContext
  runInContext (insertPackage globalKmPackageEmpty) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEmptyEvents) appContext
  runInContext (insertPackage globalKmPackage) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackage) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackageV2) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageV2Events) appContext
  runInContext (insertPackage differentPackage) appContext
  runInContext (traverse_ insertPackageEvent differentPackageEvents) appContext
  return ()

deleteAllEntitiesSql :: Query
deleteAllEntitiesSql =
  fromString . intercalate "; " . fmap ("DELETE FROM " <>) $
    [ "w_external_link_usage"
    , "w_registry_organization"
    , "w_registry_knowledge_model_package"
    , "w_registry_document_template"
    , "w_audit"
    , "w_prefab"
    , "w_persistent_command"
    , "w_submission"
    , "w_config_owl"
    , "w_config_mail"
    , "w_config_features"
    , "w_config_submission"
    , "w_config_project"
    , "w_config_registry"
    , "w_config_look_and_feel_custom_menu_link"
    , "w_config_look_and_feel"
    , "w_config_dashboard_and_login_screen_announcement"
    , "w_config_dashboard_and_login_screen"
    , "w_config_privacy_and_support"
    , "w_config_authentication"
    , "w_config_organization"
    , "w_knowledge_model_migration"
    , "w_feedback"
    , "w_user_email_link"
    , "w_knowledge_model_editor"
    , "w_document"
    , "w_document_template_draft_data"
    , "w_project_version"
    , "w_project_event"
    , "w_project_file"
    , "w_project_comment"
    , "w_project_comment_thread"
    , "w_project_perm_user"
    , "w_project_perm_group"
    , "w_project"
    , "w_document_template_file"
    , "w_document_template_asset"
    , "w_document_template"
    , "w_knowledge_model_secret"
    , "w_knowledge_model_locale"
    , "w_knowledge_model_package"
    , "w_user_token"
    , "w_user_group_membership"
    , "w_user_openid_identity"
    , "w_user_tour"
    , "w_user_entity"
    , "w_role"
    , "w_user_group"
    , "w_locale"
    , "w_tenant_limit_bundle"
    , "w_plugin"
    , "w_tenant"
    , "w_component"
    ]
