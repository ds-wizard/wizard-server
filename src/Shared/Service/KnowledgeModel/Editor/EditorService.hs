module Shared.Service.KnowledgeModel.Editor.EditorService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.Editor.EditorValidation
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.Uuid

getEditorsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page KnowledgeModelEditorList)
getEditorsPage mQuery pageable sort = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  findKnowledgeModelEditorsPage mQuery pageable sort

getEditorSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page KnowledgeModelEditorSuggestion)
getEditorSuggestionsPage mQuery pageable sort = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  findKnowledgeModelEditorSuggestionsPage mQuery pageable sort

createEditor :: WizardRequestContextC s m => KnowledgeModelEditorCreateDTO -> m KnowledgeModelEditorList
createEditor reqDto =
  runInTransaction $ do
    bUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    currentUser <- getCurrentUser
    createEditorWithParams bUuid now currentUser reqDto

createEditorWithParams :: WizardRequestContextC s m => U.UUID -> UTCTime -> UserDTO -> KnowledgeModelEditorCreateDTO -> m KnowledgeModelEditorList
createEditorWithParams uuid now currentUser reqDto =
  runInTransaction $ do
    checkKnowledgeModelEditorLimit
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    validateCreateDto reqDto
    tenantUuid <- asks (.tenantUuid')
    mPreviousPkg <-
      case reqDto.previousPackageUuid of
        Just previousPackageUuid -> do
          previousPkg <- findPackageByUuid previousPackageUuid
          when
            previousPkg.nonEditable
            (throwError . UserError $ _ERROR_SERVICE_PKG__NON_EDITABLE_PKG)
          return . Just $ previousPkg
        Nothing -> return Nothing
    let editor = fromCreateDTO reqDto uuid mPreviousPkg currentUser.uuid tenantUuid now
    insertKnowledgeModelEditor editor
    createDefaultEventIfPreviousPackageIsNotPresent editor
    return $ toList editor Nothing DefaultKnowledgeModelEditorState
  where
    createDefaultEventIfPreviousPackageIsNotPresent editor = do
      let mPreviousPackageId = editor.previousPackageUuid
      case mPreviousPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- liftIO generateUuid
          kmUuid <- liftIO generateUuid
          let addKMEvent =
                KnowledgeModelEditorEvent
                  { uuid = uuid
                  , parentUuid = U.nil
                  , entityUuid = kmUuid
                  , content = AddKnowledgeModelEvent' AddKnowledgeModelEvent {annotations = []}
                  , knowledgeModelEditorUuid = editor.uuid
                  , tenantUuid = editor.tenantUuid
                  , createdAt = now
                  }
          void $ insertKnowledgeModelEvent addKMEvent

getEditorByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelEditorDetailDTO
getEditorByUuid kmEditorUuid = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  editor <- findKnowledgeModelEditorByUuid kmEditorUuid
  editorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
  editorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
  mPreviousPackage <- traverse findPackageByUuid editor.previousPackageUuid
  mForkOfPackageId <- getEditorForkOfPackageId editor
  kmEditorState <- getEditorState editor (length editorEvents) mForkOfPackageId
  knowledgeModel <- compileKnowledgeModel [] editor.previousPackageUuid []
  mForkOfPackage <-
    case mForkOfPackageId of
      Just forkOfPackageId -> do
        pkg <- findPackageByCoordinate forkOfPackageId
        return . Just $ pkg
      Nothing -> return Nothing
  return $ toDetailDTO editor editorEvents editorReplies mPreviousPackage knowledgeModel mForkOfPackageId mForkOfPackage kmEditorState

modifyEditor :: WizardRequestContextC s m => U.UUID -> KnowledgeModelEditorChangeDTO -> m KnowledgeModelEditorDetailDTO
modifyEditor kmEditorUuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    editorFromDB <- findKnowledgeModelEditorByUuid kmEditorUuid
    validateChangeDto reqDto
    now <- liftIO getCurrentTime
    let editor =
          fromChangeDTO
            reqDto
            editorFromDB
            now
    updateKnowledgeModelEditorByUuid editor
    mForkOfPackageId <- getEditorForkOfPackageId editor
    editorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
    editorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
    mPreviousPackage <- traverse findPackageByUuid editor.previousPackageUuid
    let kmEvents = fmap toKnowledgeModelEvent editorEvents
    kmEditorState <- getEditorState editor (length editorEvents) mForkOfPackageId
    knowledgeModel <- compileKnowledgeModel kmEvents editor.previousPackageUuid []
    mForkOfPackage <-
      case mForkOfPackageId of
        Just forkOfPackageId -> do
          pkg <- findPackageByCoordinate forkOfPackageId
          return . Just $ pkg
        Nothing -> return Nothing
    return $ toDetailDTO editor editorEvents editorReplies mPreviousPackage knowledgeModel mForkOfPackageId mForkOfPackage kmEditorState

deleteEditor :: WizardRequestContextC s m => U.UUID -> m ()
deleteEditor kmEditorUuid =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    _ <- findKnowledgeModelEditorByUuid kmEditorUuid
    deleteKnowledgeModelEditorByUuid kmEditorUuid
    void $ logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid
