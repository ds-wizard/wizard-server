module Shared.Service.KnowledgeModel.Editor.Reply.EditorReplyService where

import Control.Monad (unless, void)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.ProjectReply
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper (fromSetRepliesEventDTO)
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService (broadcastReplies)
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Service.KnowledgeModel.Editor.EditorAuditLog
import Shared.Service.KnowledgeModel.Editor.EditorMapper (toReplies)
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.Project.Event.ProjectEventService (toRepliesChange)
import Shared.Service.Project.Event.ProjectEventValidation (validateReplies)
import Shared.Service.User.WizardUserMapper (toSuggestion')
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Uuid

setEditorReplies :: WizardRequestContextC s m => U.UUID -> McpRepliesChangeDTO -> m McpRepliesChangeResultDTO
setEditorReplies editorUuid reqDto = do
  checkPermissionToEditor editorUuid
  (editor, km) <- findEditorKnowledgeModel editorUuid []
  current <- toReplies <$> findKnowledgeModelRepliesByEditorUuid editorUuid
  (events, result) <- toRepliesChange (fmap (.value) current) reqDto
  validateReplies km [] (fmap (.value) current) events
  mCreatedBy <- asks (fmap toSuggestion' . (.currentUser'))
  now <- liftIO getCurrentTimeInMicroseconds
  let changed = (\value -> Reply {value = value, createdBy = mCreatedBy, createdAt = now}) <$> M.restrictKeys reqDto.replies (S.fromList result.set)
  let replies = M.union changed (M.withoutKeys current (S.fromList result.cleared))
  unless (null events) (storeEditorReplies editorUuid replies)
  al_setKnowledgeModelEditorReplies editor (M.size replies)
  return result

storeEditorReplies :: WizardRequestContextC s m => U.UUID -> M.Map String Reply -> m ()
storeEditorReplies editorUuid replies = do
  tenantUuid <- asks (.tenantUuid')
  runInTransaction . void $ updateKnowledgeModelRepliesByEditorUuid editorUuid (fromSetRepliesEventDTO editorUuid tenantUuid replies)
  uuid <- liftIO generateUuid
  broadcastReplies editorUuid (SetRepliesDTO uuid replies)
