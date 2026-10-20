module Shared.Service.KnowledgeModel.Editor.Event.EditorEventNames where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as U
import Data.UUID.V5 (generateNamed)

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Localization.LocaleRecord

createEventNames :: U.UUID -> [KnowledgeModelEditorEventChangeDTO] -> M.Map String U.UUID
createEventNames revision reqDtos = M.fromList [(reqDto.entityUuid, nameUuid revision reqDto.entityUuid) | reqDto <- reqDtos, definesName reqDto]

nameUuid :: U.UUID -> String -> U.UUID
nameUuid revision = generateNamed revision . BS.unpack . TE.encodeUtf8 . T.pack

defineName :: M.Map String U.UUID -> KnowledgeModelEditorEventChangeDTO -> M.Map String U.UUID -> M.Map String U.UUID
defineName allNames reqDto names
  | definesName reqDto = maybe names (\uuid -> M.insert reqDto.entityUuid uuid names) (M.lookup reqDto.entityUuid allNames)
  | otherwise = names

definesName :: KnowledgeModelEditorEventChangeDTO -> Bool
definesName reqDto = isSymbolicName reqDto.entityUuid && isAddEvent reqDto.content

isAddEvent :: Value -> Bool
isAddEvent (Object o) =
  case KM.lookup "eventType" o of
    Just (String eventType) -> "Add" `T.isPrefixOf` eventType
    _ -> False
isAddEvent _ = False

isSymbolicName :: String -> Bool
isSymbolicName = L.isPrefixOf "$"

resolveUuid :: M.Map String U.UUID -> String -> String -> Either [(String, LocaleRecord)] U.UUID
resolveUuid names field value
  | isSymbolicName value = maybe (Left [(field, _ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME value)]) Right (M.lookup value names)
  | otherwise = maybe (Left [(field, _ERROR_VALIDATION__KM_EVENT_INVALID_UUID value)]) Right (U.fromString value)

resolveContent :: M.Map String U.UUID -> Value -> Either [(String, LocaleRecord)] Value
resolveContent names content =
  case resolveNames names "content" False content of
    ([], resolved) -> Right resolved
    (undefinedNames, _) -> Left (fmap (fmap _ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME) undefinedNames)

resolveNames :: M.Map String U.UUID -> String -> Bool -> Value -> ([(String, String)], Value)
resolveNames names field isUuidField value =
  case value of
    String text | isUuidField && isSymbolicName (T.unpack text) -> resolveName (T.unpack text)
    Object o | isUuidField -> Object <$> traverse (resolveNames names field True) o
    Object o -> Object <$> KM.traverseWithKey (\key -> resolveNames names (field ++ "." ++ K.toString key) (isUuidKey key)) o
    Array a -> Array <$> traverse (resolveNames names field isUuidField) a
    _ -> pure value
  where
    resolveName name = maybe ([(field, name)], value) (\uuid -> ([], toJSON uuid)) (M.lookup name names)
    isUuidKey key = any (`L.isSuffixOf` K.toString key) ["Uuid", "Uuids"]
