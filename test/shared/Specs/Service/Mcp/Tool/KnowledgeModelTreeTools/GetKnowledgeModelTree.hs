module Specs.Service.Mcp.Tool.KnowledgeModelTreeTools.GetKnowledgeModelTree (
  get_knowledge_model_tree,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.UUID as U
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Resources
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.EditorMapper (toKnowledgeModelEditorEvent)
import Shared.Util.Uuid
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_knowledge_model_tree)
-- ------------------------------------------------------------------------
get_knowledge_model_tree :: RequestContext -> SpecWith ((), Application)
get_knowledge_model_tree requestContext =
  describe "POST /api/mcp (get_knowledge_model_tree)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = editorArguments ["entity_uuid" .= chapter1.uuid, "depth" .= (1 :: Int)]

reqBody = toolCallBody "get_knowledge_model_tree" reqArguments

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
            toolResult . markedTree $
              KnowledgeModelTreeDTO
                { knowledgeModelUuid = km1.uuid
                , entities =
                    [ outlineEntity chapter1.uuid (Just km1.uuid) ChapterEntityKind chapter1.title 0
                    , outlineEntity question1.uuid (Just chapter1.uuid) QuestionEntityKind question1.title 1
                    , outlineEntity question2.uuid (Just chapter1.uuid) QuestionEntityKind question2.title 1
                    ]
                }
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "get_knowledge_model_tree" structuredContent
  it "HTTP 200 OK if only the questions with the selected tags are asked for" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (editorArguments ["entity_uuid" .= chapter1.uuid, "depth" .= (1 :: Int), "tag_uuids" .= [tagDataScience.uuid]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toolResult . markedTree $
              KnowledgeModelTreeDTO
                { knowledgeModelUuid = km1.uuid
                , entities =
                    [ outlineEntity chapter1.uuid (Just km1.uuid) ChapterEntityKind chapter1.title 0
                    , outlineEntity question1.uuid (Just chapter1.uuid) QuestionEntityKind question1.title 1
                    ]
                }
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a subtree of a package is asked for with the full content" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (object ["knowledge_model_package_uuid" .= netherlandsKmPackage.uuid, "entity_uuid" .= rc1.uuid, "content" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toolResult . markedTree $
              KnowledgeModelTreeDTO
                { knowledgeModelUuid = km1.uuid
                , entities =
                    [ (outlineEntity rc1.uuid (Just km1.uuid) ResourceCollectionEntityKind rc1.title 0) {content = Just (ResourceCollectionEntity rc1)}
                    , (outlineEntity rc1_rp1.uuid (Just rc1.uuid) ResourcePageEntityKind rc1_rp1.title 1) {content = Just (ResourcePageEntity rc1_rp1)}
                    , (outlineEntity rc1_rp2.uuid (Just rc1.uuid) ResourcePageEntityKind rc1_rp2.title 1) {content = Just (ResourcePageEntity rc1_rp2)}
                    ]
                }
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "get_knowledge_model_tree" structuredContent
  it "HTTP 200 OK if the tree is too large (truncated, continued from its cursor)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (editorArguments [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelEvent (toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid longChapterEvent)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just outline = structuredContentOf response
      pages <- requestPages (editorArguments ["content" .= True]) Nothing (20 :: Int)
      liftIO $ length pages `shouldSatisfy` (> 1)
      liftIO $ fmap NE.head (NE.group (concatMap entityUuids pages)) `shouldBe` entityUuids outline
      liftIO $ fmap (valueField "truncated") pages `shouldBe` (replicate (length pages - 1) (Just (Bool True)) ++ [Nothing])
      mapM_ (assertToolOutputSchema reqHeaders "get_knowledge_model_tree") pages
  it "HTTP 200 OK if both an editor and a package are given (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (editorArguments ["knowledge_model_package_uuid" .= netherlandsKmPackage.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_ONE_OF_ARGUMENTS "knowledge_model_editor_uuid" "knowledge_model_package_uuid")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the entity does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (object ["knowledge_model_package_uuid" .= netherlandsKmPackage.uuid, "entity_uuid" .= unknownEntityUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__KM_ENTITY_ABSENCE (U.toString unknownEntityUuid))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the editor does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_knowledge_model_tree" (object ["knowledge_model_editor_uuid" .= unknownEntityUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)

editorArguments arguments = object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : arguments)

requestPages arguments mCursor remaining = do
  response <- request reqMethod reqUrl reqHeaders (toolCallBody "get_knowledge_model_tree" (withCursor mCursor arguments))
  let Just content = structuredContentOf response
  case valueField "cursor" content of
    Just cursor | remaining > 1 -> (content :) <$> requestPages arguments (Just cursor) (remaining - 1)
    _ -> return [content]

withCursor :: Maybe Value -> Value -> Value
withCursor (Just cursor) (Object o) = Object (KM.insert "cursor" cursor o)
withCursor _ arguments = arguments

outlineEntity :: U.UUID -> Maybe U.UUID -> EntityKind -> String -> Int -> KnowledgeModelEntityNodeDTO
outlineEntity uuid parentUuid entityType title depth = KnowledgeModelEntityNodeDTO {uuid = uuid, parentUuid = parentUuid, entityType = entityType, title = title, depth = depth, content = Nothing}

markedTree :: KnowledgeModelTreeDTO -> Value
markedTree dto = object ["knowledgeModelUuid" .= dto.knowledgeModelUuid, "entities" .= object ["thirdPartyContent" .= dto.entities]]

entityUuids :: Value -> [Value]
entityUuids content =
  case valueField "thirdPartyContent" =<< valueField "entities" content of
    Just (Array entities) -> mapMaybe (valueField "uuid") (V.toList entities)
    _ -> []

longChapterEvent :: KnowledgeModelEvent
longChapterEvent =
  a_km1_ch4
    { content = AddChapterEvent' (AddChapterEvent {title = "Long chapter", text = Just (replicate 30000 'x'), annotations = []})
    }

unknownEntityUuid :: U.UUID
unknownEntityUuid = u' "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
