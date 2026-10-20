module Shared.Service.KnowledgeModel.Editor.Event.EditorEventResolution where

import Control.Monad (guard)
import Control.Monad.Except (MonadError)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Either (fromLeft)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.String (fromString)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntityJM ()
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Error.Error
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.Localization.LocaleRecord
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventNames
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventValidation
import Shared.Util.Reflection (constructorName)
import Shared.Util.String (stripSuffixIfExists)

resolveEditorEvents :: MonadError AppError m => KnowledgeModel -> M.Map String U.UUID -> [KnowledgeModelEditorEventChangeDTO] -> m [KnowledgeModelEvent]
resolveEditorEvents km allNames reqDtos = do
  let (errors, events) = mconcat . snd $ L.mapAccumL (resolveEvent allNames) (km, M.empty) (zip [0 ..] reqDtos)
  throwFieldErrors errors
  return events

resolveEvent
  :: M.Map String U.UUID
  -> (KnowledgeModel, M.Map String U.UUID)
  -> (Int, KnowledgeModelEditorEventChangeDTO)
  -> ((KnowledgeModel, M.Map String U.UUID), ([(String, LocaleRecord)], [KnowledgeModelEvent]))
resolveEvent allNames (km, names) (index, reqDto) =
  let definedNames = defineName allNames reqDto names
      (nextKm, errors, events) = either (\eventErrors -> (km, eventErrors, [])) (applyAtPosition km reqDto.position) (toEvent km definedNames reqDto)
   in ((nextKm, definedNames), (fmap (first (indexedField "events" index)) errors, events))

toEvent :: KnowledgeModel -> M.Map String U.UUID -> KnowledgeModelEditorEventChangeDTO -> Either [(String, LocaleRecord)] KnowledgeModelEvent
toEvent km names reqDto =
  case (resolveUuid names "parentUuid" reqDto.parentUuid, resolveUuid names "entityUuid" reqDto.entityUuid, resolveContent names reqDto.content) of
    (Right parentUuid, Right entityUuid, Right content) -> fromEventChange parentUuid entityUuid <$> parseEventContent km entityUuid content
    (parentUuid, entityUuid, content) -> Left (fromLeft [] parentUuid ++ fromLeft [] entityUuid ++ fromLeft [] content)

parseEventContent :: KnowledgeModel -> U.UUID -> Value -> Either [(String, LocaleRecord)] KnowledgeModelEventData
parseEventContent km entityUuid =
  first (\reason -> [("content", _ERROR_VALIDATION__KM_EVENT_INVALID_CONTENT reason)]) . parseEither parseJSON . withEntityTypes km entityUuid

withEntityTypes :: KnowledgeModel -> U.UUID -> Value -> Value
withEntityTypes km uuid (Object o) = Object (KM.union o (entityTypes km uuid))
withEntityTypes _ _ content = content

entityTypes :: KnowledgeModel -> U.UUID -> Object
entityTypes km uuid =
  case toJSON <$> findEntity km uuid of
    Just (Object o) -> KM.filterWithKey (\key _ -> key `elem` ["questionType", "referenceType", "integrationType"]) o
    _ -> KM.empty

applyAtPosition :: KnowledgeModel -> Maybe Int -> KnowledgeModelEvent -> (KnowledgeModel, [(String, LocaleRecord)], [KnowledgeModelEvent])
applyAtPosition km mPosition event =
  case (errors, mPosition >>= toReorderEvent nextKm event.entityUuid) of
    ([], Just reorderEvent) -> (applyEvent nextKm reorderEvent, [], keptEvents ++ [reorderEvent])
    _ -> (nextKm, errors, keptEvents)
  where
    nextKm = applyEvent km event
    errors = validateEvent km nextKm event
    keptEvents = [event | isNothing mPosition || nextKm /= km]

toReorderEvent :: KnowledgeModel -> U.UUID -> Int -> Maybe KnowledgeModelEvent
toReorderEvent km uuid position = do
  parentUuid <- M.lookup uuid parentMap
  (field, childUuids) <- L.find (elem uuid . snd) (getChildLists km parentUuid)
  let reorderedUuids = insertAt position uuid (filter (/= uuid) childUuids)
  guard (reorderedUuids /= childUuids)
  let content = object ["eventType" .= editEventType km parentUuid, fromString field .= ChangedValue reorderedUuids]
  either (const Nothing) (Just . fromEventChange (M.findWithDefault U.nil parentUuid parentMap) parentUuid) (parseEventContent km parentUuid content)
  where
    parentMap = makeParentMap km

editEventType :: KnowledgeModel -> U.UUID -> String
editEventType km = maybe "" (\entity -> "Edit" ++ stripSuffixIfExists "Entity" (constructorName entity) ++ "Event") . findEntity km

insertAt :: Int -> a -> [a] -> [a]
insertAt position x xs = let (before, after) = splitAt position xs in before ++ x : after
