module Shared.Service.Project.Event.ProjectEventValidation where

import Control.Monad (unless, void, when)
import Control.Monad.Except (MonadError)
import Data.Bifunctor (first)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.Event.ProjectReplyValueValidation
import Shared.Service.Project.ProjectUtil
import Shared.Util.Either (maybeToEither)
import Shared.Util.String (splitOn)

validateProjectEventChanges :: WizardRequestContextC s m => Project -> [ProjectEventChangeDTO] -> m ()
validateProjectEventChanges project = validateWithKeys project False eventKey

validateProjectEventChangesWithValues :: WizardRequestContextC s m => Project -> [ProjectEventChangeDTO] -> m ()
validateProjectEventChangesWithValues project = validateWithKeys project True eventKey

validateProjectEventChange :: WizardRequestContextC s m => U.UUID -> m KnowledgeModel -> ProjectEventChangeDTO -> m ()
validateProjectEventChange projectUuid findKnowledgeModel event =
  when (hasReplyPath event) $ do
    km <- findKnowledgeModel
    validateWithKnowledgeModel projectUuid km False (const id) [event]

validateNewCommentThread :: WizardRequestContextC s m => Project -> ProjectEventChangeDTO -> m ()
validateNewCommentThread project (AddCommentEventChangeDTO' event)
  | event.newThread = do
      km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids
      throwFieldErrors [("path", error) | Left error <- [findQuestionByPath km Nothing event.path]]
validateNewCommentThread _ _ = return ()

validateWithKeys :: WizardRequestContextC s m => Project -> Bool -> (Int -> String -> String) -> [ProjectEventChangeDTO] -> m ()
validateWithKeys project withValues toKey events =
  when (any hasReplyPath events) $ do
    km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids
    validateWithKnowledgeModel project.uuid km withValues toKey events

validateWithKnowledgeModel :: WizardRequestContextC s m => U.UUID -> KnowledgeModel -> Bool -> (Int -> String -> String) -> [ProjectEventChangeDTO] -> m ()
validateWithKnowledgeModel projectUuid km withValues toKey events = do
  replies <- if any readsCurrentReplies events then findCurrentReplies projectUuid else return M.empty
  fileUuids <- findProjectFileUuidsFor projectUuid events
  throwReplyErrors toKey (validateReplyChanges km fileUuids withValues replies events)

findProjectFileUuidsFor :: WizardRequestContextC s m => U.UUID -> [ProjectEventChangeDTO] -> m [U.UUID]
findProjectFileUuidsFor projectUuid events
  | any readsProjectFiles events = fmap (.uuid) <$> findProjectFilesByProject projectUuid
  | otherwise = return []

findCurrentReplies :: WizardRequestContextC s m => U.UUID -> m (M.Map String ReplyValue)
findCurrentReplies projectUuid = fmap (.value) . (.replies) . compileProjectEvents <$> findProjectEventListsByProjectUuid projectUuid

validateReplies :: MonadError AppError m => KnowledgeModel -> [U.UUID] -> M.Map String ReplyValue -> [ProjectEventChangeDTO] -> m ()
validateReplies km fileUuids current events = throwReplyErrors (replyKey events) (validateReplyChanges km fileUuids True current events)

throwReplyErrors :: MonadError AppError m => (Int -> String -> String) -> [(Int, (String, LocaleRecord))] -> m ()
throwReplyErrors toKey = throwFieldErrors . fmap (\(index, (field, error)) -> (toKey index field, error))

orderReplies :: M.Map String ReplyValue -> [(String, ReplyValue)]
orderReplies = L.sortOn (not . isItemList . snd) . M.toList
  where
    isItemList (ItemListReply _) = True
    isItemList _ = False

eventKey :: Int -> String -> String
eventKey = indexedField "events"

replyKey :: [ProjectEventChangeDTO] -> Int -> String -> String
replyKey events index = keyedField "replies" (maybe "" changePath (events L.!? index))

changePath :: ProjectEventChangeDTO -> String
changePath (SetReplyEventChangeDTO' event) = event.path
changePath (ClearReplyEventChangeDTO' event) = event.path
changePath _ = ""

validateReplyChanges :: KnowledgeModel -> [U.UUID] -> Bool -> M.Map String ReplyValue -> [ProjectEventChangeDTO] -> [(Int, (String, LocaleRecord))]
validateReplyChanges km fileUuids withValues replies events =
  concat . snd $ L.mapAccumL validate replies (zip [0 ..] events)
  where
    validate current (index, event) = (applyReplyChange current event, toErrors index (validateReplyChange km fileUuids withValues current event))
    toErrors index = either (\error -> [(index, error)]) (const [])

validateReplyChange :: KnowledgeModel -> [U.UUID] -> Bool -> M.Map String ReplyValue -> ProjectEventChangeDTO -> Either (String, LocaleRecord) ()
validateReplyChange km fileUuids withValues replies (SetReplyEventChangeDTO' event) = do
  question <- inField "path" $ findQuestionByPath km (Just replies) event.path
  inField "value" $ do
    validateReplyType fileUuids replies question event.value
    when withValues (validateReplyValue question event.value)
validateReplyChange km _ _ _ (ClearReplyEventChangeDTO' event) = inField "path" . void $ findQuestionByPath km Nothing event.path
validateReplyChange km _ _ _ (SetLabelsEventChangeDTO' event) = inField "path" . void $ findQuestionByPath km Nothing event.path
validateReplyChange _ _ _ _ _ = Right ()

inField :: String -> Either LocaleRecord a -> Either (String, LocaleRecord) a
inField field = first ((,) field)

findQuestionByPath :: KnowledgeModel -> Maybe (M.Map String ReplyValue) -> String -> Either LocaleRecord Question
findQuestionByPath km mReplies path =
  case readReplyKey path of
    Just (chapterUuid : questionUuid : rest) -> do
      chapter <- maybeToEither pathError (M.lookup chapterUuid (getChaptersM km))
      unless (questionUuid `elem` chapter.questionUuids) (Left pathError)
      question <- maybeToEither pathError (M.lookup questionUuid (getQuestionsM km))
      followPath km mReplies pathError [chapterUuid, questionUuid] question rest
    _ -> Left pathError
  where
    pathError = _ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE path

followPath :: KnowledgeModel -> Maybe (M.Map String ReplyValue) -> LocaleRecord -> [U.UUID] -> Question -> [U.UUID] -> Either LocaleRecord Question
followPath _ _ _ _ question [] = Right question
followPath km mReplies pathError prefix question (childUuid : questionUuid : rest) = do
  questionUuids <- childQuestionUuids km mReplies pathError prefix question childUuid
  unless (questionUuid `elem` questionUuids) (Left pathError)
  nextQuestion <- maybeToEither pathError (M.lookup questionUuid (getQuestionsM km))
  followPath km mReplies pathError (prefix ++ [childUuid, questionUuid]) nextQuestion rest
followPath _ _ pathError _ _ _ = Left pathError

childQuestionUuids :: KnowledgeModel -> Maybe (M.Map String ReplyValue) -> LocaleRecord -> [U.UUID] -> Question -> U.UUID -> Either LocaleRecord [U.UUID]
childQuestionUuids km _ pathError _ (OptionsQuestion' question) answerUuid
  | answerUuid `elem` question.answerUuids = maybeToEither pathError (fmap (.followUpUuids) (M.lookup answerUuid (getAnswersM km)))
childQuestionUuids _ mReplies _ prefix (ListQuestion' question) itemUuid = do
  unless (maybe True (elem itemUuid . itemsAt (createReplyKey prefix)) mReplies) (Left $ _ERROR_VALIDATION__PROJECT_EVENT_ITEM_ABSENCE itemUuid)
  Right question.itemTemplateQuestionUuids
childQuestionUuids _ _ pathError _ _ _ = Left pathError

validateReplyType :: [U.UUID] -> M.Map String ReplyValue -> Question -> ReplyValue -> Either LocaleRecord ()
validateReplyType _ _ (OptionsQuestion' question) (AnswerReply answerUuid) =
  unless (answerUuid `elem` question.answerUuids) (Left $ _ERROR_VALIDATION__PROJECT_EVENT_ANSWER_ABSENCE answerUuid)
validateReplyType _ _ (MultiChoiceQuestion' question) (MultiChoiceReply choiceUuids) =
  maybe (Right ()) (Left . _ERROR_VALIDATION__PROJECT_EVENT_CHOICE_ABSENCE) (L.find (`notElem` question.choiceUuids) choiceUuids)
validateReplyType _ _ (ListQuestion' _) (ItemListReply _) = Right ()
validateReplyType _ _ (ValueQuestion' _) (StringReply _) = Right ()
validateReplyType _ _ (IntegrationQuestion' _) (IntegrationReply _) = Right ()
validateReplyType _ replies (ItemSelectQuestion' question) (ItemSelectReply itemUuid) =
  unless (itemUuid `elem` referencedItems replies question.listQuestionUuid) (Left $ _ERROR_VALIDATION__PROJECT_EVENT_SELECTED_ITEM_ABSENCE itemUuid)
validateReplyType fileUuids _ (FileQuestion' _) (FileReply fileUuid) =
  unless (fileUuid `elem` fileUuids) (Left $ _ERROR_VALIDATION__PROJECT_EVENT_FILE_ABSENCE fileUuid)
validateReplyType _ _ _ _ = Left _ERROR_VALIDATION__PROJECT_EVENT_REPLY_TYPE_MISMATCH

applyReplyChange :: M.Map String ReplyValue -> ProjectEventChangeDTO -> M.Map String ReplyValue
applyReplyChange replies (SetReplyEventChangeDTO' event) = M.insert event.path event.value replies
applyReplyChange replies (ClearReplyEventChangeDTO' event) = M.delete event.path replies
applyReplyChange replies _ = replies

itemsAt :: String -> M.Map String ReplyValue -> [U.UUID]
itemsAt key replies =
  case M.lookup key replies of
    Just (ItemListReply items) -> items
    _ -> []

referencedItems :: M.Map String ReplyValue -> Maybe U.UUID -> [U.UUID]
referencedItems replies (Just listQuestionUuid) = concat [items | (key, ItemListReply items) <- M.toList replies, U.toString listQuestionUuid `L.isSuffixOf` key]
referencedItems _ Nothing = []

readsCurrentReplies :: ProjectEventChangeDTO -> Bool
readsCurrentReplies (SetReplyEventChangeDTO' event) =
  case event.value of
    ItemSelectReply _ -> True
    _ -> length (splitOn "." event.path) > 2
readsCurrentReplies _ = False

readsProjectFiles :: ProjectEventChangeDTO -> Bool
readsProjectFiles (SetReplyEventChangeDTO' event) =
  case event.value of
    FileReply _ -> True
    _ -> False
readsProjectFiles _ = False

hasReplyPath :: ProjectEventChangeDTO -> Bool
hasReplyPath (SetReplyEventChangeDTO' _) = True
hasReplyPath (ClearReplyEventChangeDTO' _) = True
hasReplyPath (SetLabelsEventChangeDTO' _) = True
hasReplyPath _ = False
