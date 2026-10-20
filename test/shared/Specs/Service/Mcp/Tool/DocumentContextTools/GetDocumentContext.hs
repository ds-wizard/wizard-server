module Specs.Service.Mcp.Tool.DocumentContextTools.GetDocumentContext (
  get_document_context,
) where

import Control.Monad
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.User.RolePermission
import Shared.Service.Document.Context.DocumentContextService
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_document_context)
-- ------------------------------------------------------------------------
get_document_context :: RequestContext -> SpecWith ((), Application)
get_document_context requestContext =
  describe "POST /api/mcp (get_document_context)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["project_uuid" .= project1.uuid, "path" .= replyPath]

reqBody = toolCallBody "get_document_context" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      expCtx <- buildProjectContext requestContext Nothing
      let expDto = toolResult (object ["hash" .= expCtx.hash, "path" .= replyPath, "value" .= object ["thirdPartyContent" .= snd rQ1]])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "get_document_context" structuredContent
  it "HTTP 200 OK if the path is left out and the whole context is too large (cut with a cursor)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object ["project_uuid" .= project1.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "path" structuredContent `shouldBe` String ""
      liftIO $ jsonField "truncated" structuredContent `shouldBe` Bool True
      liftIO $ jsonField "cursor" structuredContent `shouldSatisfy` isString
  it "HTTP 200 OK if the replies are read as of an event" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object (("project_uuid" .= project1.uuid) : ["event_uuid" .= (sre_rQ2 project1.uuid).uuid, "path" .= ("/questionnaire/replies" :: String)]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ (jsonKeys . jsonField "thirdPartyContent" . jsonField "value") structuredContent `shouldBe` L.sort [fst rQ1, fst rQ2]
  it "HTTP 200 OK if a subtree is too large (cut, continued with the cursor)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (void $ insertProjectEvents [longReplyEvent]) requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "get_document_context" (object (("project_uuid" .= project1.uuid) : ["path" .= ("/questionnaire/replies" :: String)])))
      let Just firstContent = structuredContentOf firstResponse
      let reqBody = toolCallBody "get_document_context" (object (("project_uuid" .= project1.uuid) : ["path" .= ("/questionnaire/replies" :: String), "cursor" .= jsonField "cursor" firstContent]))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ jsonField "truncated" firstContent `shouldBe` Bool True
      liftIO $ toolErrorOf response `shouldBe` Nothing
      liftIO $ (jsonField "value" <$> structuredContentOf response) `shouldSatisfy` maybe False (/= Null)
      assertToolOutputSchema reqHeaders "get_document_context" firstContent
  it "HTTP 200 OK if the context is of a knowledge model editor" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid, "path" .= ("/knowledgeModelEditor/previousPackageId" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      (Right expCtx) <- runInContextIO (createKnowledgeModelEditorDocumentContext amsterdamKnowledgeModelEditor) requestContext
      let expDto = toolResult (object ["hash" .= expCtx.hash, "path" .= ("/knowledgeModelEditor/previousPackageId" :: String), "value" .= object ["thirdPartyContent" .= netherlandsKmPackage.id]])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the path picks an item of a list by its index" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid, "path" .= ("/knowledgeModel/chapterUuids/0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      (Right expCtx) <- runInContextIO (createKnowledgeModelEditorDocumentContext amsterdamKnowledgeModelEditor) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (jsonField "thirdPartyContent" . jsonField "value" <$> structuredContentOf response) `shouldBe` Just (toJSON (head expCtx.knowledgeModel.chapterUuids))
  it "HTTP 200 OK if the path does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object (("project_uuid" .= project1.uuid) : ["path" .= ("/questionnaire/nothing" :: String)]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__DOCUMENT_CONTEXT_PATH_NOT_FOUND "/questionnaire/nothing")
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if both a project and an editor are given (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object (("project_uuid" .= project1.uuid) : ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_ONE_OF_ARGUMENTS "project_uuid" "knowledge_model_editor_uuid")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if an event is given for an editor (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_context" (object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid, "event_uuid" .= (sre_rQ2 project1.uuid).uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_DEPENDENT_ARGUMENT "event_uuid" "project_uuid")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext (toolCallBody "get_document_context" (object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid])) _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION

replyPath :: String
replyPath = "/questionnaire/replies/" ++ fst rQ1

runProjectMigrations requestContext = do
  runInContextIO USR.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext

buildProjectContext requestContext mEventUuid = do
  (Right (ctx, _)) <- runInContextIO (createProjectDocumentContext project1 mEventUuid) requestContext
  return ctx

longReplyEvent :: ProjectEvent
longReplyEvent = SetReplyEvent' (sre_rQ1Updated project1.uuid) {value = StringReply (replicate 50000 'x')}

isString :: Value -> Bool
isString (String _) = True
isString _ = False

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
