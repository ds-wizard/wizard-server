module Specs.Api.Handler.KnowledgeModelEditor.Event.List_POST (
  list_POST,
) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (encode, object, toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (simpleStatus)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Internal (getApp, runWaiSession)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Resources
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Util.Date
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/knowledge-model-editors/{uuid}/events
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/knowledge-model-editors/{uuid}/events" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext
    test_409 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT editorUuid = BS.pack $ "/api/knowledge-model-editors/" ++ U.toString editorUuid ++ "/events"

reqUrl = reqUrlT amsterdamKnowledgeModelEditorList.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = amsterdamKnowledgeModelEditorEventsChange

reqEvents = [a_km1_ch4, a_km1_ch1_q2_eIsaac, m_km1_ch1_q1__to_ch2]

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 requestContext
  create_test_200_question_type_change requestContext
  create_test_200_latest_event_in_future requestContext
  create_test_200_dry_run requestContext
  create_test_200_base_revision requestContext
  create_test_200_retry requestContext
  create_test_200_events
    "HTTP 200 OK if an edit leaves an invalid value unchanged"
    requestContext
    [(editChapter1 e_km1_ch1__content {title = ChangedValue "", questionUuids = NothingChanged}) {createdAt = dt' 2019 1 1}]
    [toEventChangeDTO (editChapter1 e_km1_ch1__content {title = NothingChanged, questionUuids = NothingChanged})]
    [editChapter1 e_km1_ch1__content {title = NothingChanged, questionUuids = NothingChanged}]
  create_test_200_events
    "HTTP 200 OK if a regex validation uses JavaScript syntax"
    requestContext
    []
    [toEventChangeDTO (editQuestion1Validations [RegexQuestionValidation "^(?=.*\\d)(?:[a-z]+?)\\d{2,}$"])]
    [editQuestion1Validations [RegexQuestionValidation "^(?=.*\\d)(?:[a-z]+?)\\d{2,}$"]]
  create_test_200_names requestContext
  create_test_200_events
    "HTTP 200 OK if an edit omits unchanged fields"
    requestContext
    []
    [withoutUnchangedFields e_km1_ch1_q1]
    [e_km1_ch1_q1]
  create_test_200_events
    "HTTP 200 OK if a move states a target position"
    requestContext
    []
    [atPosition 0 (toEventChangeDTO m_km1_ch1_q1__to_ch2)]
    [m_km1_ch1_q1__to_ch2, reorderChapterQuestions chapter2 [question1.uuid, question3.uuid]]
  create_test_200_events
    "HTTP 200 OK if a reorder states a target position"
    requestContext
    []
    [ atPosition 0 (eventChange (U.toString km1.uuid) (U.toString chapter2.uuid) (object [("eventType", "EditChapterEvent")]))
    , atPosition 0 (toEventChangeDTO m_km1_ch1_q1__to_ch2 {entityUuid = question2.uuid, content = MoveQuestionEvent' (MoveQuestionEvent {targetUuid = chapter1.uuid})})
    ]
    [reorderChapters [chapter2.uuid, chapter1.uuid, chapter3.uuid], reorderChapterQuestions chapter1 [question2.uuid, question1.uuid]]
  create_test_200_history
    "HTTP 200 OK for the development history of the Amsterdam editor"
    requestContext
    amsterdamKnowledgeModelEditorList.previousPackageUuid
    amsterdamEvents
  create_test_200_history
    "HTTP 200 OK for the development history of the Netherlands packages"
    requestContext
    Nothing
    (fmap toEvent (globalKmPackageEvents ++ netherlandsKmPackageEvents ++ netherlandsKmPackageV2Events))
  create_test_200_history
    "HTTP 200 OK for the development history of the Germany package"
    requestContext
    Nothing
    germanyEvents

create_test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expOutline = amsterdamKnowledgeModelEditorEventsOutline
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.outline `shouldBe` expOutline
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents reqEvents resDto.revision
      editorFromDb <- getOneFromDB (findKnowledgeModelEditorByUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      liftIO $ editorFromDb.squashed `shouldBe` False

create_test_200_question_type_change requestContext =
  it "HTTP 200 OK if a question type change drops its children" $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChange [toEventChangeDTO e_km1_ch1_q2_type]
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents [e_km1_ch1_q2_type] resDto.revision

create_test_200_latest_event_in_future requestContext =
  it "HTTP 200 OK if the latest event is in the future" $
    -- GIVEN: Prepare expectation
    do
      let futureEditorEvent = toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid futureEvent
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelEvent futureEditorEvent) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid (amsterdamKnowledgeModelEditorEvents ++ [futureEditorEvent]) reqEvents resDto.revision

create_test_200_dry_run requestContext =
  it "HTTP 200 OK if dryRun is set" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorEventsChange {dryRun = Just True} :: KnowledgeModelEditorEventsChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expRevision = Just amsterdamRevision
      let expOutline = amsterdamKnowledgeModelEditorEventsOutline
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.revision `shouldBe` expRevision
      liftIO $ resDto.outline `shouldBe` expOutline
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents

create_test_200_base_revision requestContext =
  it "HTTP 200 OK if baseRevision is the latest event" $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChangeAt amsterdamRevision (fmap toEventChangeDTO reqEvents)
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents reqEvents resDto.revision

create_test_200_retry requestContext =
  it "HTTP 200 OK if a retried batch is already stored" $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChangeAt amsterdamRevision (symbolicNameEventChanges ++ [atPosition 0 (toEventChangeDTO m_km1_ch1_q1__to_ch2)])
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      let (_, _, expDto) = destructResponse firstResponse :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      let expEvents = symbolicNameEvents expDto.names ++ [m_km1_ch1_q1__to_ch2, reorderChapterQuestions chapter2 [question1.uuid, question3.uuid]]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents expEvents resDto.revision

create_test_200_events title requestContext storedEvents reqEvents expEvents =
  it title $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChange reqEvents
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let storedEditorEvents = fmap (toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid) storedEvents
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (traverse_ insertKnowledgeModelEvent storedEditorEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid (amsterdamKnowledgeModelEditorEvents ++ storedEditorEvents) expEvents resDto.revision

create_test_200_names requestContext =
  it "HTTP 200 OK if symbolic names are resolved in batch order" $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChange symbolicNameEventChanges
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ M.keys resDto.names `shouldBe` ["$chapter", "$question"]
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents (symbolicNameEvents resDto.names) resDto.revision

create_test_200_history title requestContext previousPackageUuid events =
  it title $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChange (fmap toEventChangeDTO events)
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runEmptyEditorMigration requestContext previousPackageUuid
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorEventsDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid [] events resDto.revision

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "events"
  create_test_400_changes
    "HTTP 400 BAD REQUEST if the batch has more than 1000 events"
    requestContext
    (replicate 1001 (toEventChangeDTO e_km1_ch1))
    [("events", _ERROR_VALIDATION__EVENT_BATCH_SIZE 1000)]
  create_test_400
    "HTTP 400 BAD REQUEST if the parent does not exist"
    requestContext
    [a_km1_ch2_q4 {parentUuid = nonExistingUuid}]
    [("events[0].parentUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString nonExistingUuid) "chapter, list question or answer")]
  create_test_400
    "HTTP 400 BAD REQUEST if the parent has a wrong type"
    requestContext
    [a_km1_ch1_q2_aMaybe {parentUuid = question1.uuid}]
    [("events[0].parentUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString question1.uuid) "options question")]
  create_test_400
    "HTTP 400 BAD REQUEST if an added entity already exists"
    requestContext
    [a_km1_ch2]
    [("events[0].entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_UNIQUENESS (U.toString chapter2.uuid))]
  create_test_400
    "HTTP 400 BAD REQUEST if an edited entity does not exist"
    requestContext
    [e_km1_ch2_q4]
    [("events[0].entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString question4.uuid) "question")]
  create_test_400
    "HTTP 400 BAD REQUEST if an entity was deleted by a preceding event"
    requestContext
    [d_km1_ch1_q1, e_km1_ch1_q1]
    [("events[1].entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString question1.uuid) "question")]
  create_test_400
    "HTTP 400 BAD REQUEST if a deleted entity has a different parent"
    requestContext
    [d_km1_ch1_q1 {parentUuid = chapter2.uuid}]
    [("events[0].parentUuid", _ERROR_VALIDATION__KM_EVENT_PARENT_MISMATCH (U.toString question1.uuid) (U.toString chapter1.uuid))]
  create_test_400
    "HTTP 400 BAD REQUEST if a move target has a wrong type"
    requestContext
    [m_km1_ch1_q1__to_ch2 {content = MoveQuestionEvent' (MoveQuestionEvent {targetUuid = tagBioinformatics.uuid})}]
    [("events[0].content.targetUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString tagBioinformatics.uuid) "chapter, list question or answer")]
  create_test_400
    "HTTP 400 BAD REQUEST if a move creates a cycle"
    requestContext
    [m_km1_ch1_q1__to_ch2 {entityUuid = question2.uuid, content = MoveQuestionEvent' (MoveQuestionEvent {targetUuid = q2_answerYes.uuid})}]
    [("events[0].content.targetUuid", _ERROR_VALIDATION__KM_EVENT_MOVE_CYCLE (U.toString question2.uuid) (U.toString q2_answerYes.uuid))]
  create_test_400
    "HTTP 400 BAD REQUEST if a referenced tag and phase do not exist"
    requestContext
    [d_km1_tds, d_km1_phs1, d_km1_ch1_q1, a_km1_ch1_q1]
    [ ("events[3].content.tagUuids", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString tagDataScience.uuid) "tag")
    , ("events[3].content.requiredPhaseUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString phase1.uuid) "phase")
    ]
  create_test_400
    "HTTP 400 BAD REQUEST if a referenced integration does not exist"
    requestContext
    [d_km1_ir, a_km1_ch3_q9]
    [("events[1].content.integrationUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString repositoryApi.uuid) "integration")]
  create_test_400
    "HTTP 400 BAD REQUEST if a referenced list question does not exist"
    requestContext
    [a_km1_ch3_q13]
    [("events[0].content.listQuestionUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString question4.uuid) "list question")]
  create_test_400
    "HTTP 400 BAD REQUEST if a referenced metric does not exist"
    requestContext
    [d_km1_mtrF, a_km1_ch1_q2_aNo1 {entityUuid = newEntityUuid}]
    [("events[1].content.metricMeasures", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString metricF.uuid) "metric")]
  create_test_400
    "HTTP 400 BAD REQUEST if a cross-reference target does not exist"
    requestContext
    [a_km1_ch1_q2_rCh3]
    [("events[0].content.targetUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString km1_ch1_q2_r3.targetUuid) "entity")]
  create_test_400
    "HTTP 400 BAD REQUEST if a referenced resource page does not exist"
    requestContext
    [d_km1_rc1_rp1, a_km1_ch2_q6_rCh1 {parentUuid = question3.uuid}]
    [("events[1].content.resourcePageUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString rc1_rp1.uuid) "resource page")]
  create_test_400
    "HTTP 400 BAD REQUEST if an edited child list is not a reordering of the current children"
    requestContext
    [e_km1_ch1 {content = EditChapterEvent' e_km1_ch1__content {questionUuids = ChangedValue [question2.uuid]}}]
    [("events[0].content.questionUuids", _ERROR_VALIDATION__KM_EVENT_CHILDREN_PERMUTATION (L.intercalate ", " (fmap U.toString [question1.uuid, question2.uuid])))]
  create_test_400
    "HTTP 400 BAD REQUEST if a required text is empty"
    requestContext
    [ editChapter1 e_km1_ch1__content {title = ChangedValue "  "}
    , editAnswerYes e_km1_ch1_q2_aYes1_2__content {aLabel = ChangedValue ""}
    ]
    [ ("events[0].content.title", _ERROR_VALIDATION__KM_EVENT_EMPTY_TEXT)
    , ("events[1].content.label", _ERROR_VALIDATION__KM_EVENT_EMPTY_TEXT)
    ]
  create_test_400
    "HTTP 400 BAD REQUEST if a URL is invalid"
    requestContext
    [e_km1_ch1_q2_rCh2 {content = EditReferenceEvent' (EditURLReferenceEvent' e_km1_ch1_q2_rCh2__content {url = ChangedValue "wizard.org/fair"})}]
    [("events[0].content.url", _ERROR_VALIDATION__KM_EVENT_INVALID_URL "wizard.org/fair")]
  create_test_400
    "HTTP 400 BAD REQUEST if an e-mail is invalid"
    requestContext
    [e_km1_ch1_q2_eAlbert {content = EditExpertEvent' e_km1_ch1_q2_eAlbert__content {email = ChangedValue "albert.einstein"}}]
    [("events[0].content.email", _ERROR_VALIDATION__KM_EVENT_INVALID_EMAIL "albert.einstein")]
  create_test_400
    "HTTP 400 BAD REQUEST if a regex validation does not compile"
    requestContext
    [editQuestion1Validations [RegexQuestionValidation "[a-z"]]
    [("events[0].content.validations", _ERROR_VALIDATION__KM_EVENT_INVALID_REGEX "[a-z")]
  create_test_400
    "HTTP 400 BAD REQUEST if a lower limit is greater than an upper limit"
    requestContext
    [ editQuestion1Validations [MinLengthQuestionValidation 10, MaxLengthQuestionValidation 5]
    , editQuestion1
        e_km1_ch1_q1__content
          { valueType = ChangedValue DateQuestionValueType
          , validations = ChangedValue [FromDateQuestionValidation "2024-02-01", ToDateQuestionValidation "2024-01-31"]
          }
    ]
    [ ("events[0].content.validations", _ERROR_VALIDATION__KM_EVENT_INVERTED_RANGE "10" "5")
    , ("events[1].content.validations", _ERROR_VALIDATION__KM_EVENT_INVERTED_RANGE "2024-02-01" "2024-01-31")
    ]
  create_test_400
    "HTTP 400 BAD REQUEST if a metric measure or weight is not between 0 and 1"
    requestContext
    [editAnswerYes e_km1_ch1_q2_aYes1_2__content {metricMeasures = ChangedValue [metricMeasureI1 {measure = 1.5, weight = -0.5}]}]
    [ ("events[0].content.metricMeasures", _ERROR_VALIDATION__KM_EVENT_OUT_OF_UNIT_RANGE "1.5")
    , ("events[0].content.metricMeasures", _ERROR_VALIDATION__KM_EVENT_OUT_OF_UNIT_RANGE "-0.5")
    ]
  create_test_400
    "HTTP 400 BAD REQUEST if a validation does not match the value type"
    requestContext
    [editQuestion1Validations [MinNumberQuestionValidation 1]]
    [("events[0].content.validations", _ERROR_VALIDATION__KM_EVENT_VALIDATION_VALUE_TYPE "MinNumberQuestionValidation" "StringQuestionValueType")]
  create_test_400
    "HTTP 400 BAD REQUEST with every error and nothing stored"
    requestContext
    [a_km1_ch4, a_km1_ch2, e_km1_ch2_q4]
    [ ("events[1].entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_UNIQUENESS (U.toString chapter2.uuid))
    , ("events[2].entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString question4.uuid) "question")
    ]
  create_test_400_changes
    "HTTP 400 BAD REQUEST if a symbolic name is not defined by a preceding Add event"
    requestContext
    [ eventChange "$chapter" "$question" (withContentField "tagUuids" (toJSON ["$tag" :: String]) a_km1_ch1_q1)
    , eventChange (U.toString km1.uuid) "$chapter" (toJSON a_km1_ch4.content)
    ]
    [ ("events[0].parentUuid", _ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME "$chapter")
    , ("events[0].content.tagUuids", _ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME "$tag")
    ]
  create_test_400_changes
    "HTTP 400 BAD REQUEST if an entity UUID is neither a UUID nor a symbolic name"
    requestContext
    [eventChange (U.toString km1.uuid) "chapter-4" (toJSON a_km1_ch4.content)]
    [("events[0].entityUuid", _ERROR_VALIDATION__KM_EVENT_INVALID_UUID "chapter-4")]
  create_test_400_changes
    "HTTP 400 BAD REQUEST if the event content cannot be read"
    requestContext
    [eventChange (U.toString km1.uuid) (U.toString chapter1.uuid) (object [("eventType", "RenameChapterEvent")])]
    [("events[0].content", _ERROR_VALIDATION__KM_EVENT_INVALID_CONTENT "Error in $: One of the events has unsupported eventType: RenameChapterEvent")]

  it "HTTP 400 BAD REQUEST if baseRevision does not exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChangeAt nonExistingUuid (fmap toEventChangeDTO reqEvents)
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__UNKNOWN_BASE_REVISION (U.toString nonExistingUuid))
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 400 BAD REQUEST if the editor has a migration" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__MIGRATION_IN_PROGRESS
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runEditorMigrationMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents

create_test_400 title requestContext events = create_test_400_changes title requestContext (fmap toEventChangeDTO events)

create_test_400_changes title requestContext reqEvents expErrors =
  it title $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChange reqEvents
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.fromListWith (flip (++)) . fmap (fmap pure) $ expErrors)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/knowledge-model-editors/dc9fe65f-748b-47ec-b30c-d255bbac64a0/events"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_409 requestContext = do
  create_test_409
    "HTTP 409 CONFLICT if baseRevision is not the latest event"
    requestContext
    amsterdamRevision
    [futureEvent]
    [futureEvent]
  create_test_409_concurrent requestContext

create_test_409 title requestContext baseRevision addedEvents expEvents =
  it title $
    -- GIVEN: Prepare request
    do
      let reqDto = eventsChangeAt baseRevision (fmap toEventChangeDTO reqEvents)
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let addedEditorEvents = fmap (toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid) addedEvents
      let expStatus = 409
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ConflictError (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__OUTDATED_BASE_REVISION (U.toString baseRevision)) (toJSON expEvents)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (traverse_ insertKnowledgeModelEvent addedEditorEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid (amsterdamKnowledgeModelEditorEvents ++ addedEditorEvents)

create_test_409_concurrent requestContext =
  it "HTTP 409 CONFLICT for all but one of concurrent batches on the same base revision" $
    -- GIVEN: Prepare request
    do
      let reqBodies = fmap (\event -> encode (eventsChangeAt amsterdamRevision [toEventChangeDTO event])) reqEvents
      -- AND: Prepare expectation
      let expStatuses = [200, 409, 409]
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      app <- getApp
      responses <- liftIO $ mapConcurrently (\reqBody -> runWaiSession (request reqMethod reqUrl reqHeaders reqBody) app) reqBodies
      -- THEN: Compare response with expectation
      liftIO $ L.sort (fmap (statusCode . simpleStatus) responses) `shouldBe` expStatuses
      -- AND: Compare state in DB with expectation
      eEvents <- runInContextIO (findKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      liftIO $ fmap length eEvents `shouldBe` Right (length amsterdamKnowledgeModelEditorEvents + 1)
