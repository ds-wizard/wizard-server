module Specs.Api.Handler.KnowledgeModelEditor.Event.Common where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Tenant
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Util.Date
import Shared.Util.List (lastSafe)
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Common

-- --------------------------------
-- MIGRATION
-- --------------------------------
runEmptyEditorMigration requestContext previousPackageUuid = do
  let reqDto = amsterdamKnowledgeModelEditorCreate {previousPackageUuid = previousPackageUuid} :: KnowledgeModelEditorCreateDTO
  runInContextIO
    ( createEditorWithParams
        amsterdamKnowledgeModelEditorList.uuid
        amsterdamKnowledgeModelEditorList.createdAt
        (fromJust requestContext.currentUser)
        reqDto
    )
    requestContext
  runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext

runEditorMigrationMigration :: MonadIO m => RequestContext -> m (Either AppError ())
runEditorMigrationMigration = runInContextIO (void $ insertKnowledgeModelMigration amsterdamKnowledgeModelMigration)

-- --------------------------------
-- FIXTURES
-- --------------------------------
nonExistingUuid :: U.UUID
nonExistingUuid = u' "4b3c5b0a-8a51-4bd4-9f43-31d6a3d0a2f1"

amsterdamKnowledgeModelMigration :: KnowledgeModelMigration
amsterdamKnowledgeModelMigration =
  differentKnowledgeModelMigration
    { editorUuid = amsterdamKnowledgeModelEditorList.uuid
    , editorPreviousPackageUuid = netherlandsKmPackage.uuid
    , targetPackageUuid = netherlandsKmPackageV2.uuid
    , tenantUuid = defaultTenant.uuid
    }
    :: KnowledgeModelMigration

newEntityUuid :: U.UUID
newEntityUuid = u' "9c0f6a3e-2b7d-4e1a-8f55-0d3c6b2a7e94"

futureEvent :: KnowledgeModelEvent
futureEvent = a_km1_ch1_q2_aMaybe {createdAt = dt' 2100 1 1}

amsterdamRevision :: U.UUID
amsterdamRevision = (last amsterdamKnowledgeModelEditorEvents).uuid

eventsChange :: [KnowledgeModelEditorEventChangeDTO] -> KnowledgeModelEditorEventsChangeDTO
eventsChange events = KnowledgeModelEditorEventsChangeDTO {events = events, baseRevision = Nothing, dryRun = Nothing}

eventsChangeAt :: U.UUID -> [KnowledgeModelEditorEventChangeDTO] -> KnowledgeModelEditorEventsChangeDTO
eventsChangeAt baseRevision events = KnowledgeModelEditorEventsChangeDTO {events = events, baseRevision = Just baseRevision, dryRun = Nothing}

editChapter1 :: EditChapterEvent -> KnowledgeModelEvent
editChapter1 content = e_km1_ch1 {content = EditChapterEvent' content}

editQuestion1 :: EditValueQuestionEvent -> KnowledgeModelEvent
editQuestion1 content = e_km1_ch1_q1 {content = EditQuestionEvent' (EditValueQuestionEvent' content)}

editQuestion1Validations :: [QuestionValidation] -> KnowledgeModelEvent
editQuestion1Validations validations = editQuestion1 e_km1_ch1_q1__content {validations = ChangedValue validations}

editAnswerYes :: EditAnswerEvent -> KnowledgeModelEvent
editAnswerYes content = e_km1_ch1_q2_aYes1_2 {content = EditAnswerEvent' content}

eventChange :: String -> String -> Value -> KnowledgeModelEditorEventChangeDTO
eventChange parentUuid entityUuid content = KnowledgeModelEditorEventChangeDTO parentUuid entityUuid content Nothing

atPosition :: Int -> KnowledgeModelEditorEventChangeDTO -> KnowledgeModelEditorEventChangeDTO
atPosition position reqDto = KnowledgeModelEditorEventChangeDTO reqDto.parentUuid reqDto.entityUuid reqDto.content (Just position)

withContentField :: Key -> Value -> KnowledgeModelEvent -> Value
withContentField key value event =
  case toJSON event.content of
    Object o -> Object (KM.insert key value o)
    content -> content

withoutUnchangedFields :: KnowledgeModelEvent -> KnowledgeModelEditorEventChangeDTO
withoutUnchangedFields event =
  case toJSON event.content of
    Object o -> eventChange (U.toString event.parentUuid) (U.toString event.entityUuid) (Object . KM.filter isChanged . KM.delete "questionType" $ o)
    content -> eventChange (U.toString event.parentUuid) (U.toString event.entityUuid) content
  where
    isChanged value = value /= object ["changed" .= False]

symbolicNameEventChanges :: [KnowledgeModelEditorEventChangeDTO]
symbolicNameEventChanges =
  [ eventChange (U.toString km1.uuid) "$chapter" (toJSON a_km1_ch4.content)
  , eventChange "$chapter" "$question" (toJSON a_km1_ch1_q1.content)
  , eventChange (U.toString chapter1.uuid) (U.toString question2.uuid) (withContentField "targetUuid" "$chapter" m_km1_ch1_q1__to_ch2)
  ]

symbolicNameEvents :: M.Map String U.UUID -> [KnowledgeModelEvent]
symbolicNameEvents names =
  [ fromEventChange km1.uuid (name "$chapter") a_km1_ch4.content
  , fromEventChange (name "$chapter") (name "$question") a_km1_ch1_q1.content
  , fromEventChange chapter1.uuid question2.uuid (MoveQuestionEvent' (MoveQuestionEvent {targetUuid = name "$chapter"}))
  ]
  where
    name = (names M.!)

reorderChapterQuestions :: Chapter -> [U.UUID] -> KnowledgeModelEvent
reorderChapterQuestions chapter questionUuids =
  fromEventChange km1.uuid chapter.uuid . EditChapterEvent' $
    EditChapterEvent
      { title = NothingChanged
      , text = NothingChanged
      , annotations = NothingChanged
      , questionUuids = ChangedValue questionUuids
      }

reorderChapters :: [U.UUID] -> KnowledgeModelEvent
reorderChapters chapterUuids =
  fromEventChange U.nil km1.uuid . EditKnowledgeModelEvent' $
    EditKnowledgeModelEvent
      { annotations = NothingChanged
      , chapterUuids = ChangedValue chapterUuids
      , tagUuids = NothingChanged
      , integrationUuids = NothingChanged
      , metricUuids = NothingChanged
      , phaseUuids = NothingChanged
      , resourceCollectionUuids = NothingChanged
      }

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertAppendedEventsInDB :: RequestContext -> U.UUID -> [KnowledgeModelEditorEvent] -> [KnowledgeModelEvent] -> Maybe U.UUID -> WaiSession () ()
assertAppendedEventsInDB requestContext editorUuid existingEvents expEvents revision = do
  eEvents <- runInContextIO (findKnowledgeModelEventsByEditorUuid editorUuid) requestContext
  liftIO $ isRight eEvents `shouldBe` True
  let (Right events) = eEvents
  let (oldEvents, newEvents) = splitAt (length existingEvents) events
  let createdAts = fmap (.createdAt) events
  liftIO $ fmap (.uuid) oldEvents `shouldBe` fmap (.uuid) existingEvents
  liftIO $ fmap toChange newEvents `shouldBe` fmap toChange expEvents
  liftIO $ and (zipWith (<) createdAts (drop 1 createdAts)) `shouldBe` True
  liftIO $ revision `shouldBe` fmap (.uuid) (lastSafe events)
  where
    toChange event = (event.parentUuid, event.entityUuid, event.content)

assertEventUuidsInDB :: RequestContext -> U.UUID -> [KnowledgeModelEditorEvent] -> WaiSession () ()
assertEventUuidsInDB requestContext editorUuid existingEvents = do
  eEvents <- runInContextIO (findKnowledgeModelEventsByEditorUuid editorUuid) requestContext
  liftIO $ isRight eEvents `shouldBe` True
  let (Right events) = eEvents
  liftIO $ fmap (.uuid) events `shouldBe` fmap (.uuid) existingEvents
