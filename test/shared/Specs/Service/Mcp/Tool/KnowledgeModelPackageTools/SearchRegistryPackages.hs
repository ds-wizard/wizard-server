module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.SearchRegistryPackages (
  search_registry_packages,
) where

import Data.Aeson
import qualified Data.List as L
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Constant.Tenant
import Shared.Database.DAO.Settings.SettingsRegistryDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Registry.RegistryPackage
import Shared.Model.Settings.Settings
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (search_registry_packages)
-- ------------------------------------------------------------------------
search_registry_packages :: RequestContext -> SpecWith ((), Application)
search_registry_packages requestContext =
  describe "POST /api/mcp (search_registry_packages)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object []

reqBody = toolCallBody "search_registry_packages" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toolResult $
              object
                [ "packages"
                    .= [ catalogItem "knowledgeModel" globalRegistryPackage.id globalKmPackage.name globalKmPackage.description globalRegistryPackage.remoteVersion
                       , catalogItem "knowledgeModel" nlRegistryPackage.id netherlandsKmPackageV2.name netherlandsKmPackageV2.description nlRegistryPackage.remoteVersion
                       , catalogItem "documentTemplate" wizardDocumentTemplate.id wizardDocumentTemplate.name wizardDocumentTemplate.description wizardDocumentTemplate.version
                       ]
                ]
      -- AND: Run migrations
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "search_registry_packages" structuredContent
  it "HTTP 200 OK if the query matches a package name" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "search_registry_packages" (object ["q" .= ("netherlands" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (catalogIds =<< structuredContentOf response) `shouldBe` Just [String (T.pack nlRegistryPackage.id)]
  it "HTTP 200 OK if only document templates are asked for" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "search_registry_packages" (object ["kind" .= ("documentTemplate" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (catalogIds =<< structuredContentOf response) `shouldBe` Just [String (T.pack wizardDocumentTemplate.id)]
  it "HTTP 200 OK if the registry is disabled (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED "Registry")
      -- AND: Run migrations
      runInContextIO (saveSettingsRegistry defaultTenantUuid (settingsRegistry {enabled = False})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the tools are listed (the registry tools are open-world)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let openWorld name = valueField "openWorldHint" =<< valueField "annotations" =<< L.find ((== Just (String name)) . valueField "name") (toolsOf response)
      liftIO $ fmap openWorld ["search_registry_packages", "pull_knowledge_model_package", "get_knowledge_model_package"] `shouldBe` [Just (Bool True), Just (Bool True), Just (Bool False)]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
