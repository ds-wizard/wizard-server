module Specs.Service.Mcp.Tool.KnowledgeModelEditorReplyTools.Common where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil

import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
valuePath :: String
valuePath = createReplyKey [chapter1.uuid, question1.uuid]

optionsPath :: String
optionsPath = createReplyKey [chapter1.uuid, question2.uuid]

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertKnowledgeModelEditorRepliesInDB :: RequestContext -> U.UUID -> M.Map String ReplyValue -> WaiSession () ()
assertKnowledgeModelEditorRepliesInDB requestContext editorUuid expReplies = do
  replies <- getOneFromDB (findKnowledgeModelRepliesByEditorUuid editorUuid) requestContext
  liftIO $ M.fromList (fmap (\reply -> (reply.path, reply.value)) replies) `shouldBe` expReplies
