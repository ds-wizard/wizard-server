module Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.Common where

import Data.Aeson (Value (..))
import qualified Data.UUID as U
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newEditorUuid :: U.UUID
newEditorUuid = u' "2c4e6a8b-0d1f-4a3c-9e5b-7d9f1b3d5f70"

firstEditorUpdatedAt :: Value -> Maybe Value
firstEditorUpdatedAt content =
  case valueField "knowledgeModelEditors" =<< valueField "_embedded" content of
    Just (Array editors) | not (V.null editors) -> valueField "updatedAt" (V.head editors)
    _ -> Nothing

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfKnowledgeModelEditorInDB :: RequestContext -> KnowledgeModelEditor -> WaiSession () ()
assertExistenceOfKnowledgeModelEditorInDB requestContext expEditor = do
  editor <- getOneFromDB (findKnowledgeModelEditorByUuid expEditor.uuid) requestContext
  liftIO $ editor `shouldBe` expEditor {createdAt = editor.createdAt, updatedAt = editor.updatedAt}

assertKnowledgeModelEditorsInDB :: RequestContext -> [KnowledgeModelEditor] -> WaiSession () ()
assertKnowledgeModelEditorsInDB requestContext expEditors = do
  editors <- getOneFromDB findKnowledgeModelEditors requestContext
  liftIO $ editors `shouldBe` zipWith (\editor expEditor -> expEditor {uuid = editor.uuid, createdAt = editor.createdAt, updatedAt = editor.updatedAt}) editors expEditors

assertInitialEventOfKnowledgeModelEditorInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertInitialEventOfKnowledgeModelEditorInDB requestContext editorUuid = do
  event <- getFirstFromDB (findKnowledgeModelEventsByEditorUuid editorUuid) requestContext
  liftIO $ event.entityUuid `shouldSatisfy` (/= U.nil)
