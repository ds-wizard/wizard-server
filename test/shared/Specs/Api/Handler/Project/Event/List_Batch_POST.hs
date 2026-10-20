module Specs.Api.Handler.Project.Event.List_Batch_POST (
  list_batch_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.ProjectEventBatches
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Util.Date
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/projects/{uuid}/events/batch
-- ------------------------------------------------------------------------
list_batch_POST :: RequestContext -> SpecWith ((), Application)
list_batch_POST requestContext =
  describe "POST /api/projects/{uuid}/events/batch" $ do
    test_201 requestContext
    test_400 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/events/batch"

reqUrl = reqUrlT project1.uuid

reqHeadersT authHeader = reqCtHeader : authHeader

reqHeaders = reqHeadersT [reqAuthHeader]

reqDto = ProjectContentChangeDTO {events = batchEvents}

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 requestContext
  create_test_201_value "HTTP 201 CREATED with a number" requestContext NumberQuestionValueType [MinNumberQuestionValidation 1, MaxNumberQuestionValidation 5] "3.14"
  create_test_201_value "HTTP 201 CREATED with a date" requestContext DateQuestionValueType [FromDateQuestionValidation "2018-01-01", ToDateQuestionValidation "2018-12-31"] "2018-06-01"
  create_test_201_value "HTTP 201 CREATED with a date time" requestContext DateTimeQuestionValueType [FromDateTimeQuestionValidation "2018-01-21T10:00"] "2018-01-21T10:30"
  create_test_201_value "HTTP 201 CREATED with a date time in UTC" requestContext DateTimeQuestionValueType [] "2018-01-21T10:30:00.000Z"
  create_test_201_value "HTTP 201 CREATED with a time" requestContext TimeQuestionValueType [FromTimeQuestionValidation "09:00", ToTimeQuestionValidation "17:00"] "10:00"
  create_test_201_value "HTTP 201 CREATED with an email" requestContext EmailQuestionValueType [DomainQuestionValidation "example.com, example.org"] "albert.einstein@mail.example.org"
  create_test_201_value "HTTP 201 CREATED with a URL" requestContext UrlQuestionValueType [] "https://example.com/path?query=1"
  create_test_201_value "HTTP 201 CREATED with a color" requestContext ColorQuestionValueType [] "#00aaFF"
  create_test_201_value "HTTP 201 CREATED with a text" requestContext TextQuestionValueType [MinLengthQuestionValidation 2, MaxLengthQuestionValidation 3, RegexQuestionValidation "^\\d+$"] "123"
  create_test_201_value "HTTP 201 CREATED with a regex with a non-capturing group" requestContext StringQuestionValueType [RegexQuestionValidation "^(?:ab)+$"] "abab"
  create_test_201_value "HTTP 201 CREATED with a DOI" requestContext StringQuestionValueType [DoiQuestionValidation] "10.1000/xyz123"
  create_test_201_value "HTTP 201 CREATED with an ORCID" requestContext StringQuestionValueType [OrcidQuestionValidation] "https://orcid.org/0000-0002-1825-009X"

create_test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = fmap (\event -> toEventDTO' event (Just userAlbertSuggestion) (dt' 2018 1 21)) batchEvents
      let expType (a :: [ProjectEventDTO]) = a
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (insertProjectFile projectFile) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertListResponseWithoutFields expStatus expHeaders expDto expType response ["createdAt"]
      -- AND: Compare state in DB with expectation
      eEventsFromDb <- runInContextIO (findProjectEventsByProjectUuid project1.uuid) requestContext
      liftIO $ fmap (fmap toEventChangeDTO) eEventsFromDb `shouldBe` Right (fmap toEventChangeDTO project1Events ++ batchEvents)

create_test_201_value title requestContext valueType validations value =
  it title $
    -- GIVEN: Prepare request
    do
      let event = SetReplyEventChangeDTO' (srche_rQ9 {value = StringReply value} :: SetReplyEventChangeDTO)
      let reqBody = encode (ProjectContentChangeDTO {events = [event]})
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = [toEventDTO' event (Just userAlbertSuggestion) (dt' 2018 1 21)]
      let expType (a :: [ProjectEventDTO]) = a
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (insertPackageEvent (q9AsValueQuestionEvent valueType validations)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertListResponseWithoutFields expStatus expHeaders expDto expType response ["createdAt"]
      -- AND: Compare state in DB with expectation
      eEventsFromDb <- runInContextIO (findProjectEventsByProjectUuid project1.uuid) requestContext
      liftIO $ fmap (fmap toEventChangeDTO) eEventsFromDb `shouldBe` Right (fmap toEventChangeDTO project1Events ++ [event])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "events"
  create_test_400
    "HTTP 400 BAD REQUEST if the batch has more than 1000 events"
    requestContext
    (replicate 1001 (SetReplyEventChangeDTO' srche_rQ1))
    [("events", _ERROR_VALIDATION__EVENT_BATCH_SIZE 1000)]
  create_test_400
    "HTTP 400 BAD REQUEST if paths don't match the knowledge model"
    requestContext
    [ SetReplyEventChangeDTO' (srche_rQ2 {path = createReplyKey [chapter1.uuid, question3.uuid]} :: SetReplyEventChangeDTO)
    , SetReplyEventChangeDTO' srche_rQ1
    , SetReplyEventChangeDTO' (srche_rQ2 {path = "not-a-path"} :: SetReplyEventChangeDTO)
    ]
    [ ("events[0].path", _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE (createReplyKey [chapter1.uuid, question3.uuid]))
    , ("events[2].path", _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE "not-a-path")
    ]
  create_test_400
    "HTTP 400 BAD REQUEST if an answer in the path doesn't belong to the question"
    requestContext
    [SetReplyEventChangeDTO' (srche_rQ2_aYes_fuQ1 {path = createReplyKey [chapter1.uuid, question2.uuid, q3_answerNo.uuid, q2_aYes_fuQuestion1.uuid]} :: SetReplyEventChangeDTO)]
    [("events[0].path", _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE (createReplyKey [chapter1.uuid, question2.uuid, q3_answerNo.uuid, q2_aYes_fuQuestion1.uuid]))]
  create_test_400
    "HTTP 400 BAD REQUEST if a list item in the path doesn't exist"
    requestContext
    [SetReplyEventChangeDTO' srche_rQ4_it3_q6]
    [("events[0].path", _ERROR_VALIDATION__PROJECT_EVENT_ITEM_ABSENCE rQ4_it3)]
  create_test_400
    "HTTP 400 BAD REQUEST if a cleared path doesn't match the knowledge model"
    requestContext
    [ClearReplyEventChangeDTO' (crche_rQ9 {path = createReplyKey [chapter1.uuid, question9.uuid]} :: ClearReplyEventChangeDTO)]
    [("events[0].path", _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE (createReplyKey [chapter1.uuid, question9.uuid]))]
  create_test_400
    "HTTP 400 BAD REQUEST if a labelled path doesn't match the knowledge model"
    requestContext
    [SetLabelsEventChangeDTO' (slblche_rQ2 {path = createReplyKey [chapter2.uuid, question2.uuid]} :: SetLabelsEventChangeDTO)]
    [("events[0].path", _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE (createReplyKey [chapter2.uuid, question2.uuid]))]
  create_test_400
    "HTTP 400 BAD REQUEST if the reply doesn't match the question type"
    requestContext
    [SetReplyEventChangeDTO' (srche_rQ2 {value = StringReply "Yes"} :: SetReplyEventChangeDTO)]
    [("events[0].value", _ERROR_VALIDATION__PROJECT_EVENT_REPLY_TYPE_MISMATCH)]
  create_test_400
    "HTTP 400 BAD REQUEST if the answer doesn't belong to the question"
    requestContext
    [SetReplyEventChangeDTO' (srche_rQ2 {value = AnswerReply q3_answerNo.uuid} :: SetReplyEventChangeDTO)]
    [("events[0].value", _ERROR_VALIDATION__PROJECT_EVENT_ANSWER_ABSENCE q3_answerNo.uuid)]
  create_test_400
    "HTTP 400 BAD REQUEST if a choice doesn't belong to the question"
    requestContext
    [SetReplyEventChangeDTO' (srche_rQ10 {value = MultiChoiceReply [q11_choice1.uuid, q2_answerNo.uuid]} :: SetReplyEventChangeDTO)]
    [("events[0].value", _ERROR_VALIDATION__PROJECT_EVENT_CHOICE_ABSENCE q2_answerNo.uuid)]
  create_test_400
    "HTTP 400 BAD REQUEST if the selected item doesn't exist"
    requestContext
    [SetReplyEventChangeDTO' srche_rQ13]
    [("events[0].value", _ERROR_VALIDATION__PROJECT_EVENT_SELECTED_ITEM_ABSENCE rQ4_it3)]
  create_test_400
    "HTTP 400 BAD REQUEST if the file doesn't belong to the project"
    requestContext
    [SetReplyEventChangeDTO' srche_rQ14]
    [("events[0].value", _ERROR_VALIDATION__PROJECT_EVENT_FILE_ABSENCE projectFile.uuid)]
  create_test_400_value "HTTP 400 BAD REQUEST if a number isn't valid" requestContext NumberQuestionValueType [] "3,14" (valueTypeError NumberQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if a date isn't valid" requestContext DateQuestionValueType [] "21.1.2018" (valueTypeError DateQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if a date time isn't valid" requestContext DateTimeQuestionValueType [] "yesterday" (valueTypeError DateTimeQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if a time isn't valid" requestContext TimeQuestionValueType [] "noon" (valueTypeError TimeQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if an email isn't valid" requestContext EmailQuestionValueType [] "albert.einstein" (valueTypeError EmailQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if a URL isn't valid" requestContext UrlQuestionValueType [] "www.example.com" (valueTypeError UrlQuestionValueType)
  create_test_400_value "HTTP 400 BAD REQUEST if a color isn't valid" requestContext ColorQuestionValueType [] "red" (valueTypeError ColorQuestionValueType)
  create_test_400_validation "HTTP 400 BAD REQUEST if a value is too short" requestContext StringQuestionValueType (MinLengthQuestionValidation 5) "abc"
  create_test_400_validation "HTTP 400 BAD REQUEST if a value is too long" requestContext StringQuestionValueType (MaxLengthQuestionValidation 2) "abc"
  create_test_400_validation "HTTP 400 BAD REQUEST if a value doesn't match the regex" requestContext StringQuestionValueType (RegexQuestionValidation "^\\d+$") "abc"
  create_test_400_validation "HTTP 400 BAD REQUEST if a value doesn't match a regex with a non-capturing group" requestContext StringQuestionValueType (RegexQuestionValidation "^(?:ab)+$") "abc"
  create_test_400_validation "HTTP 400 BAD REQUEST if the regex does not compile" requestContext StringQuestionValueType (RegexQuestionValidation "(ab") "ab"
  create_test_400_validation "HTTP 400 BAD REQUEST if a value isn't an ORCID" requestContext StringQuestionValueType OrcidQuestionValidation "0000-0002"
  create_test_400_validation "HTTP 400 BAD REQUEST if a value isn't a DOI" requestContext StringQuestionValueType DoiQuestionValidation "doi:10"
  create_test_400_validation "HTTP 400 BAD REQUEST if a number is too small" requestContext NumberQuestionValueType (MinNumberQuestionValidation 10) "5"
  create_test_400_validation "HTTP 400 BAD REQUEST if a number is too big" requestContext NumberQuestionValueType (MaxNumberQuestionValidation 1) "5"
  create_test_400_validation "HTTP 400 BAD REQUEST if a date is too early" requestContext DateQuestionValueType (FromDateQuestionValidation "2018-01-21") "2018-01-20"
  create_test_400_validation "HTTP 400 BAD REQUEST if a date is too late" requestContext DateQuestionValueType (ToDateQuestionValidation "2018-01-21") "2018-01-22"
  create_test_400_validation "HTTP 400 BAD REQUEST if a date time is too early" requestContext DateTimeQuestionValueType (FromDateTimeQuestionValidation "2018-01-21T10:00") "2018-01-21T09:59"
  create_test_400_validation "HTTP 400 BAD REQUEST if a date time is too late" requestContext DateTimeQuestionValueType (ToDateTimeQuestionValidation "2018-01-21T10:00") "2018-01-21T10:01"
  create_test_400_validation "HTTP 400 BAD REQUEST if a time is too early" requestContext TimeQuestionValueType (FromTimeQuestionValidation "10:00") "09:00"
  create_test_400_validation "HTTP 400 BAD REQUEST if a time is too late" requestContext TimeQuestionValueType (ToTimeQuestionValidation "10:00") "11:00"
  create_test_400_validation "HTTP 400 BAD REQUEST if an email is from another domain" requestContext EmailQuestionValueType (DomainQuestionValidation "example.com") "albert@example.org"
  it "HTTP 400 BAD REQUEST if a later event fails, which stores none of the batch" $
    -- GIVEN: Prepare request
    do
      let reqEvents = [AddCommentEventChangeDTO' (newThreadComment newThreadCommentUuid1), AddCommentEventChangeDTO' (newThreadComment newThreadCommentUuid2)]
      let reqBody = encode (ProjectContentChangeDTO {events = reqEvents})
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION
      let expBody = encode expDto
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById newThreadUuid) requestContext
      liftIO $ fmap (.uuid) mThread `shouldBe` Nothing

create_test_400 title requestContext events expErrors =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = encode (ProjectContentChangeDTO {events = events})
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.fromList (fmap (fmap (: [])) expErrors))
      let expBody = encode expDto
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectContentInDB requestContext project1.uuid project1Events

create_test_400_validation title requestContext valueType validation value =
  create_test_400_value title requestContext valueType [validation] value (_ERROR_VALIDATION__PROJECT_EVENT_VALUE_VALIDATION (show validation))

create_test_400_value title requestContext valueType validations value expError =
  it title $
    -- GIVEN: Prepare request
    do
      let event = SetReplyEventChangeDTO' (srche_rQ9 {value = StringReply value} :: SetReplyEventChangeDTO)
      let reqBody = encode (ProjectContentChangeDTO {events = [event]})
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "events[0].value" [expError])
      let expBody = encode expDto
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (insertPackageEvent (q9AsValueQuestionEvent valueType validations)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectContentInDB requestContext project1.uuid project1Events

valueTypeError = _ERROR_VALIDATION__PROJECT_EVENT_VALUE_TYPE . show

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" requestContext project1 [reqNonAdminAuthHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" requestContext project2 [reqNonAdminAuthHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Anonymous, Public)" requestContext project3 [] _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project authHeader reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError reason
      let expBody = encode expDto
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectContentInDB requestContext project.uuid (fEvents project.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  createNotFoundTest'
    reqMethod
    (reqUrlT (u' "f08ead5f-746d-411b-aee6-77ea3d24016a"))
    reqHeaders
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
  it "HTTP 404 NOT FOUND if the comment thread belongs to another project" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project2.uuid
      let reqBody = encode (ProjectContentChangeDTO {events = [rtche_rQ1_t1']})
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment_thread" [("project_uuid", U.toString project2.uuid), ("uuid", U.toString cmtQ1_t1.uuid)]
      let expBody = encode expDto
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById cmtQ1_t1.uuid) requestContext
      liftIO $ fmap (.resolved) mThread `shouldBe` Just False

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
newThreadComment :: U.UUID -> AddCommentEventChangeDTO
newThreadComment commentUuid = acche_rQ2_t1_1 {uuid = commentUuid, threadUuid = newThreadUuid, commentUuid = commentUuid}

newThreadUuid :: U.UUID
newThreadUuid = u' "3c1d9e2a-6b7f-4a8e-9d0c-5e4f3a2b1c0d"

newThreadCommentUuid1 :: U.UUID
newThreadCommentUuid1 = u' "4d2e0f3b-7c8a-4b9f-8e1d-6f5a4b3c2d1e"

newThreadCommentUuid2 :: U.UUID
newThreadCommentUuid2 = u' "5e3f1a4c-8d9b-4caf-9f2e-7a6b5c4d3e2f"

runProjectMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
