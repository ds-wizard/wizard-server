module Main where

import Control.Concurrent.MVar
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Test.Hspec

import Shared.Cache.CacheFactory
import Shared.Constant.Resource
import Shared.Constant.Tenant
import Shared.Database.Connection
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Integration.Http.Common.HttpClientFactory
import Shared.Integration.Http.Common.ServantClient
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.S3.Common
import Shared.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Service.Config.Server.ServerConfigService
import Shared.Service.User.WizardUserMapper
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext
import WizardServer.Service.Config.Server.ServerConfigValidation

import Specs.Api.Handler.ApiKey.ApiSpec
import Specs.Api.Handler.Config.ApiSpec
import Specs.Api.Handler.Document.ApiSpec
import Specs.Api.Handler.DocumentTemplate.ApiSpec
import Specs.Api.Handler.DocumentTemplateDraft.ApiSpec
import Specs.Api.Handler.DocumentTemplateDraft.Asset.ApiSpec
import Specs.Api.Handler.DocumentTemplateDraft.File.ApiSpec
import Specs.Api.Handler.DocumentTemplateDraft.Folder.ApiSpec
import Specs.Api.Handler.Domain.ApiSpec
import Specs.Api.Handler.ExternalLink.ApiSpec
import Specs.Api.Handler.Info.ApiSpec
import Specs.Api.Handler.KnowledgeModel.ApiSpec
import Specs.Api.Handler.KnowledgeModelEditor.ApiSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.WebsocketSpec
import Specs.Api.Handler.KnowledgeModelPackage.ApiSpec
import Specs.Api.Handler.KnowledgeModelSecret.ApiSpec
import Specs.Api.Handler.Locale.ApiSpec
import Specs.Api.Handler.OpenIdClient.ApiSpec
import Specs.Api.Handler.Prefab.ApiSpec
import Specs.Api.Handler.Project.ApiSpec
import Specs.Api.Handler.Project.Detail_WS.WebsocketSpec
import Specs.Api.Handler.ProjectCommentThread.ApiSpec
import Specs.Api.Handler.Role.ApiSpec
import Specs.Api.Handler.Submission.ApiSpec
import Specs.Api.Handler.Swagger.ApiSpec
import Specs.Api.Handler.Tenant.ApiSpec
import Specs.Api.Handler.Tenant.Config.ApiSpec
import Specs.Api.Handler.Tenant.Limit.ApiSpec
import Specs.Api.Handler.Tenant.Usage.ApiSpec
import Specs.Api.Handler.Token.ApiSpec
import Specs.Api.Handler.TypeHint.ApiSpec
import Specs.Api.Handler.User.ApiSpec
import Specs.Api.Handler.User.News.ApiSpec
import Specs.Api.Handler.User.Tour.ApiSpec
import Specs.Api.Handler.UserGroup.ApiSpec
import Specs.Api.Handler.Websocket.Common
import Specs.Integration.Http.Common.ResponseMapperSpec
import Specs.Integration.Http.Common.SsrfProtectionSpec
import Specs.Model.Common.PageSpec
import Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Specs.Service.Coordinate.CoordinateValidationSpec
import Specs.Service.Document.DocumentServiceSpec
import Specs.Service.DocumentTemplate.DocumentTemplateUtilSpec
import Specs.Service.KnowledgeModel.Compiler.CompilerSpec
import Specs.Service.KnowledgeModel.Compiler.Modifier.ModifierSpec
import Specs.Service.KnowledgeModel.Editor.KnowledgeModelEditorServiceSpec
import Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec
import Specs.Service.KnowledgeModel.Locale.Pot.PotFileServiceSpec
import Specs.Service.KnowledgeModel.Metamodel.Migrator.EventMigratorSpec
import Specs.Service.KnowledgeModel.Migration.Migrator.MigrationSpec
import qualified Specs.Service.KnowledgeModel.Migration.Migrator.SanitizerSpec as KM_SanitizerSpec
import Specs.Service.KnowledgeModel.Package.PackageUtilSpec
import Specs.Service.KnowledgeModel.Package.PackageValidationSpec
import Specs.Service.KnowledgeModel.Squash.SquasherSpec
import Specs.Service.PersistentCommand.PersistentCommandServiceSpec
import Specs.Service.Project.Cache.ProjectCacheServiceSpec
import Specs.Service.Project.Collaboration.ProjectCollaborationAclSpec
import Specs.Service.Project.Compiler.ProjectCompilerServiceSpec
import Specs.Service.Project.Event.ProjectEventServiceSpec
import qualified Specs.Service.Project.Migration.Migrator.ChangeQTypeSanitizerSpec as PRJ_ChangeQTypeSanitizer
import qualified Specs.Service.Project.Migration.Migrator.MoveSanitizerSpec as PRJ_MoveSanitizerSpec
import qualified Specs.Service.Project.Migration.Migrator.SanitizerSpec as PRJ_SanitizerSpec
import Specs.Service.Project.ProjectAclSpec
import Specs.Service.Project.ProjectServiceSpec
import Specs.Service.Project.ProjectValidationSpec
import Specs.Service.Report.ReportGeneratorSpec
import Specs.Service.Tenant.Config.TenantConfigValidationSpec
import Specs.Service.Tenant.TenantValidationSpec
import Specs.Service.User.UserServiceSpec
import Specs.Util.GettextSpec
import Specs.Util.JinjaSpec
import Specs.Util.ListSpec
import Specs.Util.MapSpec
import Specs.Util.MathSpec
import Specs.Util.StringSpec
import Specs.Util.TokenSpec
import TestMigration

hLoadConfig fileName loadFn callback = do
  eitherConfig <- loadFn fileName
  case eitherConfig of
    Left error -> do
      putStrLn $ "CONFIG: load failed (" ++ fileName ++ ")"
      putStrLn $ "CONFIG: can't load " ++ fileName ++ ". Maybe the file is missing or not well-formatted"
      putStrLn $ "CONFIG: " ++ show error
    Right config -> do
      putStrLn $ "CONFIG: '" ++ fileName ++ "' loaded"
      callback config

prepareWebApp runCallback =
  hLoadConfig serverConfigFileTest (BS.readFile >=> getServerConfig validateServerConfig) $ \serverConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      shutdownFlag <- newEmptyMVar
      putStrLn $ "ENVIRONMENT: set to " `mappend` serverConfig.general.environment
      dbPool <- createDatabaseConnectionPool serverConfig.database
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager serverConfig.logging
      putStrLn "HTTP_CLIENT: created"
      restrictedHttpClientManager <- createRestrictedHttpClientManager serverConfig.logging serverConfig.httpClient.restricted.allowedHosts
      putStrLn "RESTRICTED_HTTP_CLIENT: created"
      s3Client <- createS3Client serverConfig.s3 httpClientManager
      putStrLn "S3_CLIENT: created"
      registryClient <- createRegistryClient serverConfig httpClientManager
      putStrLn "REGISTRY_CLIENT: created"
      cache <- createServerCache serverConfig
      putStrLn "CACHE: created"
      let serverContext =
            ServerContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
              , restrictedHttpClientManager = restrictedHttpClientManager
              , registryClient = registryClient
              , shutdownFlag = shutdownFlag
              , cache = cache
              }
      withResource dbPool $ \dbConnection -> do
        breadcrumbs <- newIORef []
        let requestContext =
              RequestContext
                { serverConfig = serverConfig
                , buildInfoConfig = buildInfoConfig
                , dbPool = dbPool
                , dbConnection = Just dbConnection
                , s3Client = s3Client
                , httpClientManager = httpClientManager
                , restrictedHttpClientManager = restrictedHttpClientManager
                , registryClient = registryClient
                , traceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
                , breadcrumbs = breadcrumbs
                , currentTenantUuid = defaultTenantUuid
                , currentUser = Just . toDTO $ userAlbert
                , shutdownFlag = shutdownFlag
                , cache = cache
                }
        putStrLn "DB: start creating schema"
        buildSchema requestContext
        putStrLn "DB: schema created"
        runWebserver requestContext (runCallback serverContext requestContext)

main :: IO ()
main =
  prepareWebApp
    ( \serverContext requestContext ->
        hspec $ do
          describe "UNIT TESTING" $ do
            describe "INTEGRATION" $
              describe "Http" $ do
                describe "Common" commonResponseMapperSpec
                describe "Common" ssrfProtectionSpec
            describe "MODEL" $ do
              pageSpec
              knowledgeModelAccessorsSpec
            describe "SERVICE" $ do
              describe "Coordinate" coordinateValidationSpec
              describe "Document Template" documentTemplateUtilSpec
              describe "KnowledgeModel" $ do
                describe "Metamodel" $
                  describe
                    "Migration"
                    eventMigratorSpec
                describe "Compiler" $ do
                  describe "Modifier" modifierSpec
                  compilerSpec
                describe "Locale" potFileServiceSpec
                describe "Package" packageUtilSpec
                describe "Squash" $ do squasherSpec
                knowledgeModelFilterSpec
              describe "Migration" $ do
                describe "Project" $ describe "Migration" $ do
                  PRJ_ChangeQTypeSanitizer.sanitizerSpec
                  PRJ_MoveSanitizerSpec.sanitizerSpec
              describe "Project" $ do
                describe "Event" $ do
                  projectCompilerServiceSpec
                  projectEventServiceSpec
                projectValidationSpec
              describe "Report" reportGeneratorSpec
              describe "Tenant" $ do
                describe "Config" tenantConfigValidationSpec
                tenantValidationSpec
            describe "UTIL" $ do
              mapSpec
              mathSpec
              listSpec
              stringSpec
              tokenSpec
              jinjaSpec
              gettextSpec
          before (resetDB requestContext) $ describe "INTEGRATION TESTING" $ do
            describe "API" $ do
              apiKeyAPI serverContext requestContext
              configAPI serverContext requestContext
              documentAPI serverContext requestContext
              documentTemplateAPI serverContext requestContext
              documentTemplateDraftAPI serverContext requestContext
              documentTemplateDraftFolderAPI serverContext requestContext
              documentTemplateDraftAssetAPI serverContext requestContext
              documentTemplateDraftFileAPI serverContext requestContext
              domainAPI serverContext requestContext
              externalLinkAPI serverContext requestContext
              infoAPI serverContext requestContext
              knowledgeModelAPI serverContext requestContext
              knowledgeModelEditorAPI serverContext requestContext
              knowledgeModelPackageAPI serverContext requestContext
              knowledgeModelSecretAPI serverContext requestContext
              localeAPI serverContext requestContext
              openIdClientAPI serverContext requestContext
              prefabAPI serverContext requestContext
              projectAPI serverContext requestContext
              projectCommentThreadAPI serverContext requestContext
              submissionAPI serverContext requestContext
              swaggerAPI serverContext requestContext
              tenantAPI serverContext requestContext
              tenantConfigAPI serverContext requestContext
              tenantLimitAPI serverContext requestContext
              typeHintAPI serverContext requestContext
              tokenAPI serverContext requestContext
              usageAPI serverContext requestContext
              userAPI serverContext requestContext
              userNewsAPI serverContext requestContext
              userTourAPI serverContext requestContext
              roleAPI serverContext requestContext
              userGroupAPI serverContext requestContext
            describe "SERVICE" $ do
              documentIntegrationSpec requestContext
              describe "KnowledgeModel" $ do
                describe "Editor" $ do
                  describe "Migration" $ do
                    migratorSpec requestContext
                    KM_SanitizerSpec.sanitizerSpec requestContext
                  knowledgeModelEditorServiceSpec requestContext
                describe "Package" $ packageValidationSpec requestContext
              persistentCommandServiceSpec requestContext
              describe "Project" $ do
                describe "Migration" $
                  PRJ_SanitizerSpec.sanitizerIntegrationSpec requestContext
                projectAclSpec requestContext
                projectCacheServiceSpec requestContext
                projectCollaborationAclSpec requestContext
                projectServiceSpec requestContext
              userServiceIntegrationSpec requestContext
            describe "WEBSOCKET" $ do
              knowledgeModelEditorWebsocketAPI requestContext
              projectWebsocketAPI requestContext
    )
