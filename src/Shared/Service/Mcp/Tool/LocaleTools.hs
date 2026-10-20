module Shared.Service.Mcp.Tool.LocaleTools where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Proxy
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Locale.Common.LocaleUploadService (maxLocalePoSize)
import Shared.Service.Locale.LocaleService
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

localeTools :: WizardRequestContextC s m => [McpTool m]
localeTools = [listLocalesTool, getLocaleTranslation, saveLocale, deleteLocaleTool]

listLocalesTool :: WizardRequestContextC s m => McpTool m
listLocalesTool =
  McpTool
    { name = "list_locales"
    , title = "List locales"
    , description = "List the UI locales users can switch the interface to, with whether each is enabled, which one is the default and whether it came from the registry (official)"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ localesSchema (Proxy :: Proxy LocaleDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = localeReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\locales -> object ["locales" .= locales]) <$> getLocales
    }

getLocaleTranslation :: WizardRequestContextC s m => McpTool m
getLocaleTranslation =
  markThirdPartyFields [["content"]] $
    McpTool
      { name = "get_locale_translation"
      , title = "Get locale translation"
      , description =
          "Get the PO file of a UI locale (listed by list_locales) to review or edit it and save it again with save_locale; "
            ++ "long content is cut, call it again with the cursor of the result to read on"
      , inputSchema = argumentsSchema localeUuidArgument
      , outputSchema = Just translationSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = localeReadPermissions
      , toolset = Just AdminMcpToolset
      , transactionState = Transactional
      , run = withArguments localeUuidArgument $ fmap translationResult . getLocalePoContent
      }

saveLocale :: WizardRequestContextC s m => McpTool m
saveLocale =
  McpTool
    { name = "save_locale"
    , title = "Save locale"
    , description = saveLocaleDescription "UI locale" "list_locales"
    , inputSchema = argumentsSchema (confirmationArguments saveLocaleArguments)
    , outputSchema = Just $ saveLocaleSchema (Proxy :: Proxy LocaleDTO)
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = localeWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmationWhen "save_locale" saveLocaleArguments (prepareLocaleSave uploadLocale modifyLocaleFlags)
    }

deleteLocaleTool :: WizardRequestContextC s m => McpTool m
deleteLocaleTool =
  McpTool
    { name = "delete_locale"
    , title = "Delete locale"
    , description = "Delete a UI locale; users who chose it fall back to the default locale"
    , inputSchema = argumentsSchema localeUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = localeWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments localeUuidArgument $ \uuid -> object [] <$ deleteLocale uuid
    }

localeReadPermissions :: [String]
localeReadPermissions = [_MCP_READ_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]

localeWritePermissions :: [String]
localeWritePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]

localeUuidArgument :: Arguments U.UUID
localeUuidArgument = requiredArgument "locale_uuid" "The locale"

localesSchema :: JsonSchema locale => Proxy locale -> Value
localesSchema proxy = objectSchema ["locales"] [("locales", arraySchema (jsonSchema proxy))]

translationSchema :: Value
translationSchema = objectSchema ["content"] [("content", stringSchema)]

translationResult :: BS.ByteString -> Value
translationResult content = object ["content" .= decodeText content]

saveLocaleDescription :: String -> String -> String
saveLocaleDescription entityName listTool =
  "Upload a PO file (content) as a "
    ++ entityName
    ++ "; its Language header gives the code and a new locale is stored disabled. "
    ++ "Replacing the locale of the same code needs confirmation: the call then returns a summary and a confirmation_token, show the summary to the user and repeat the same call with the token. "
    ++ "enabled and default_locale switch the locale on or off and make it the default (which enables it); without content they change the locale uuid (from "
    ++ listTool
    ++ ")"

saveLocaleArguments :: Arguments (Maybe U.UUID, Maybe BS.ByteString, Maybe Bool, Maybe Bool)
saveLocaleArguments =
  (,,,)
    <$> optionalArgument "uuid" "The locale whose enabled or default_locale to change when no content is given"
    <*> textContentArgument maxLocalePoSize "content" "The PO file, at most 5 MB"
    <*> optionalArgument "enabled" "Whether users can choose the locale"
    <*> optionalArgument "default_locale" "Make it the default locale of the organization"

saveLocaleSchema :: JsonSchema locale => Proxy locale -> Value
saveLocaleSchema proxy =
  confirmationOutputSchema $
    objectSchema ["locale"] [("locale", jsonSchema proxy), ("replaced", describedSchema "Whether a locale of the same code was replaced" (jsonSchema (Proxy :: Proxy Bool)))]

prepareLocaleSave
  :: (WizardRequestContextC s m, ToJSON locale, HasField "uuid" locale U.UUID)
  => (LocaleUploadDTO -> m (LocaleUploadResultDTO locale))
  -> (U.UUID -> Maybe Bool -> Maybe Bool -> m locale)
  -> (Maybe U.UUID, Maybe BS.ByteString, Maybe Bool, Maybe Bool)
  -> m (Either (m Value) (McpConfirmation m))
prepareLocaleSave upload modifyFlags (_, Just content, mEnabled, mDefaultLocale) = prepareLocaleUpload (upload . LocaleUploadDTO content) modifyFlags mEnabled mDefaultLocale
prepareLocaleSave _ modifyFlags (Just uuid, Nothing, mEnabled, mDefaultLocale) = return . Left $ (\locale -> object ["locale" .= locale]) <$> modifyFlags uuid mEnabled mDefaultLocale
prepareLocaleSave _ _ _ = throwError . UserError $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "locale" ["content"]

prepareLocaleUpload
  :: (WizardRequestContextC s m, ToJSON locale, HasField "uuid" locale U.UUID)
  => (Bool -> m (LocaleUploadResultDTO locale))
  -> (U.UUID -> Maybe Bool -> Maybe Bool -> m locale)
  -> Maybe Bool
  -> Maybe Bool
  -> m (Either (m Value) (McpConfirmation m))
prepareLocaleUpload upload modifyFlags mEnabled mDefaultLocale = do
  preview <- upload False
  let perform = storeLocaleUpload upload modifyFlags mEnabled mDefaultLocale
  return $ case preview of
    PreviewLocaleUploadResultDTO name code _ warnings@(_ : _) ->
      Right McpConfirmation {summary = replaceSummary name code warnings, revision = show warnings, perform = perform}
    _ -> Left perform

storeLocaleUpload
  :: (WizardRequestContextC s m, ToJSON locale, HasField "uuid" locale U.UUID)
  => (Bool -> m (LocaleUploadResultDTO locale))
  -> (U.UUID -> Maybe Bool -> Maybe Bool -> m locale)
  -> Maybe Bool
  -> Maybe Bool
  -> m Value
storeLocaleUpload upload modifyFlags mEnabled mDefaultLocale = do
  result <- upload True
  case result of
    StoredLocaleUploadResultDTO replaced locale -> do
      updated <- applyLocaleFlags modifyFlags locale mEnabled mDefaultLocale
      return $ object ["locale" .= updated, "replaced" .= replaced]
    _ -> return $ object []

applyLocaleFlags :: (Monad m, HasField "uuid" locale U.UUID) => (U.UUID -> Maybe Bool -> Maybe Bool -> m locale) -> locale -> Maybe Bool -> Maybe Bool -> m locale
applyLocaleFlags _ locale Nothing Nothing = return locale
applyLocaleFlags modifyFlags locale mEnabled mDefaultLocale = modifyFlags locale.uuid mEnabled mDefaultLocale

replaceSummary :: String -> String -> [LocaleUploadWarningDTO] -> String
replaceSummary name code warnings =
  "Replaces the locale " ++ intercalate ", " (fmap warningName warnings) ++ " with the uploaded " ++ name ++ " (" ++ code ++ "); its current translations are overwritten"
  where
    warningName warning = warning.name ++ (if warning.official then " (official, from the registry)" else "")
