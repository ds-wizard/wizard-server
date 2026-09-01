module Shared.Service.Project.Migration.Migrator.Sanitizer (
  sanitizeProjectEvents,
) where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Time

import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Service.Project.Compiler.ProjectCompilerService
import qualified Shared.Service.Project.Migration.Migrator.ChangeQTypeSanitizer as CTS
import qualified Shared.Service.Project.Migration.Migrator.MoveSanitizer as MS
import Shared.Service.User.WizardUserMapper
import Shared.Util.Uuid

sanitizeProjectEvents :: WizardRequestContextC s m => KnowledgeModel -> KnowledgeModel -> [ProjectEventList] -> m [ProjectEventList]
sanitizeProjectEvents oldKm newKm events = do
  let oldProjectContent = compileProjectEvents events
  let oldReplies = oldProjectContent.replies
  now <- liftIO getCurrentTime
  let sanitizedReplies = M.fromList . sanitizeReplies now oldKm newKm . M.toList $ oldReplies
  clearReplyEvents <- generateClearReplyEvents oldReplies sanitizedReplies
  setReplyEvents <- generateSetReplyEvents oldReplies sanitizedReplies
  return $ clearReplyEvents ++ setReplyEvents

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm = MS.sanitizeReplies now oldKm newKm . CTS.sanitizeReplies newKm

generateClearReplyEvents :: WizardRequestContextC s m => M.Map String Reply -> M.Map String Reply -> m [ProjectEventList]
generateClearReplyEvents oldReplies sanitizedReplies = traverse generateEvent repliesToBeDeleted
  where
    repliesToBeDeleted :: [ReplyTuple]
    repliesToBeDeleted = M.toList . M.filterWithKey (\k _ -> k `M.notMember` sanitizedReplies) $ oldReplies
    generateEvent (k, _) = do
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return . ClearReplyEventList' $ ClearReplyEventList eUuid k (Just (toSuggestion' user)) now

generateSetReplyEvents :: WizardRequestContextC s m => M.Map String Reply -> M.Map String Reply -> m [ProjectEventList]
generateSetReplyEvents oldReplies sanitizedReplies = foldl generateEvent (return []) (M.toList sanitizedReplies)
  where
    generateEvent accM (keyFromSanitizedReply, valueFromSanitizedReply) = do
      acc <- accM
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return $
        case M.lookup keyFromSanitizedReply oldReplies of
          Just valueFromOldReply ->
            if valueFromOldReply.value == valueFromSanitizedReply.value
              then acc
              else
                acc
                  ++ [ SetReplyEventList' $
                         SetReplyEventList
                           eUuid
                           keyFromSanitizedReply
                           valueFromSanitizedReply.value
                           (Just (toSuggestion' user))
                           now
                     ]
          Nothing ->
            acc
              ++ [ SetReplyEventList' $ SetReplyEventList eUuid keyFromSanitizedReply valueFromSanitizedReply.value (Just (toSuggestion' user)) now
                 ]
