module Shared.Service.Mcp.Tool.McpWriteToolCommon where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.Uuid

writeAnnotations :: McpToolAnnotations
writeAnnotations = McpToolAnnotations {readOnlyHint = False, destructiveHint = False, idempotentHint = False, openWorldHint = False}

idempotentWriteAnnotations :: McpToolAnnotations
idempotentWriteAnnotations = writeAnnotations {idempotentHint = True}

destructiveAnnotations :: McpToolAnnotations
destructiveAnnotations = McpToolAnnotations {readOnlyHint = False, destructiveHint = True, idempotentHint = True, openWorldHint = False}

uuidDescription :: String -> String
uuidDescription entity = "Give uuid to change " ++ entity ++ ", or with a new UUID to make a create safe to repeat"

secretDescription :: String
secretDescription = "the value is never returned: outputs show ******** instead, and ******** sent back keeps the stored value"

expectedUpdatedAtDescription :: String -> String
expectedUpdatedAtDescription entityName =
  "Give expected_updated_at (the updatedAt you last saw) to refuse the change when someone changed the " ++ entityName ++ " since"

uuidArgument :: String -> Arguments (Maybe U.UUID)
uuidArgument entityName =
  optionalArgument "uuid" ("The " ++ entityName ++ " to change; for a new one the UUID it gets, so that repeating the same call creates it only once")

expectedUpdatedAtArgument :: Arguments (Maybe UTCTime)
expectedUpdatedAtArgument = optionalArgument "expected_updated_at" "The updatedAt you last saw; the change is refused when someone changed it since"

changesArguments :: Value -> Arguments Object
changesArguments (Object schema)
  | Just (Object properties) <- KM.lookup "properties" schema =
      Arguments [(K.toString key, property) | (key, property) <- KM.toList properties] [] (return . KM.filterWithKey (\key _ -> KM.member key properties))
changesArguments _ = pure KM.empty

saveByUuid :: WizardRequestContextC s m => (U.UUID -> m (Maybe e)) -> (U.UUID -> m a) -> (e -> m a) -> Maybe U.UUID -> m a
saveByUuid _ create _ Nothing = create =<< liftIO generateUuid
saveByUuid find create update (Just uuid) = maybe (create uuid) update =<< find uuid

checkUnchangedSince :: WizardRequestContextC s m => String -> Maybe UTCTime -> UTCTime -> m ()
checkUnchangedSince entityName (Just expected) updatedAt
  | updatedAt > expected = throwError . UserError $ _ERROR_SERVICE_MCP__CHANGED_SINCE entityName (iso8601Show updatedAt)
checkUnchangedSince _ _ _ = return ()

requireChanges :: WizardRequestContextC s m => String -> [String] -> Object -> m ()
requireChanges entityName names changes =
  case filter (not . (`KM.member` changes) . K.fromString) names of
    [] -> return ()
    missing -> throwError . UserError $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS entityName missing

createFromChanges :: WizardRequestContextC s m => String -> [String] -> (Object -> m dto) -> (dto -> m a) -> Object -> m a
createFromChanges entityName names parse create changes = do
  requireChanges entityName names changes
  create =<< parse changes

mergeChangesSince :: (WizardRequestContextC s m, ToJSON a, FromJSON a) => String -> Maybe UTCTime -> UTCTime -> Object -> a -> m a
mergeChangesSince entityName mExpectedUpdatedAt updatedAt changes current = do
  checkUnchangedSince entityName mExpectedUpdatedAt updatedAt
  mergeChanges changes current

modifyIfChanged :: (Monad m, ToJSON dto) => dto -> dto -> m a -> (dto -> m a) -> m a
modifyIfChanged current changed unchanged modify
  | toJSON changed == toJSON current = unchanged
  | otherwise = modify changed

mergeChanges :: (WizardRequestContextC s m, ToJSON a, FromJSON a) => Object -> a -> m a
mergeChanges changes current =
  case toJSON current of
    Object fields -> parseChanges (KM.union changes fields)
    _ -> parseChanges changes

mergeNestedChanges :: (WizardRequestContextC s m, ToJSON a, FromJSON a) => Object -> a -> m a
mergeNestedChanges changes current =
  case toJSON current of
    Object fields -> parseChanges (mergeObjects changes fields)
    _ -> parseChanges changes

mergeObjects :: Object -> Object -> Object
mergeObjects = KM.unionWith mergeValues
  where
    mergeValues (Object changes) (Object fields) = Object (mergeObjects changes fields)
    mergeValues changed _ = changed

parseChanges :: (WizardRequestContextC s m, FromJSON a) => Object -> m a
parseChanges changes =
  case fromJSON (Object changes) of
    Success value -> return value
    Error message -> throwError . UserError $ _ERROR_SERVICE_MCP__INVALID_ARGUMENTS message
