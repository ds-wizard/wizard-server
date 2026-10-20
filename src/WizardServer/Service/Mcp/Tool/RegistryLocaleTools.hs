module WizardServer.Service.Mcp.Tool.RegistryLocaleTools where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Proxy

import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocaleJM ()
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO (MailTemplateLocaleDTO (..))
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Locale.LocaleService (modifyLocaleFlags)
import Shared.Service.Locale.Registry.RegistryLocaleService
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService (modifyMailTemplateLocaleFlags)
import Shared.Service.MailTemplateLocale.Registry.RegistryMailTemplateLocaleService
import Shared.Service.Mcp.Tool.LocaleTools
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon

registryLocaleTools :: WizardRequestContextC s m => [McpTool m]
registryLocaleTools = [listRegistryLocales, pullRegistryLocaleTool]

listRegistryLocales :: WizardRequestContextC s m => McpTool m
listRegistryLocales =
  McpTool
    { name = "list_registry_locales"
    , title = "List registry locales"
    , description =
        "List the official UI locales, or with mail_templates the mail template locales, the registry offers, "
          ++ "with their latest version and whether a locale of that code is installed; install one with pull_registry_locale"
    , inputSchema = argumentsSchema mailTemplatesArgument
    , outputSchema = Just $ localesSchema (Proxy :: Proxy RegistryLocaleDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = localeReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments mailTemplatesArgument $ fmap (\locales -> object ["locales" .= locales]) . registryLocales
    }
  where
    registryLocales False = getRegistryLocales
    registryLocales True = getRegistryMailTemplateLocales

pullRegistryLocaleTool :: WizardRequestContextC s m => McpTool m
pullRegistryLocaleTool =
  McpTool
    { name = "pull_registry_locale"
    , title = "Pull registry locale"
    , description =
        "Install the latest version of an official locale from the registry by its code (from list_registry_locales), a UI locale or with mail_templates a mail template locale; "
          ++ "a new locale is stored disabled, enabled and default_locale switch it on and make it the default. "
          ++ "Replacing an installed locale of the same code needs confirmation: the call then returns a summary and a confirmation_token, show the summary to the user and repeat the same call with the token"
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ saveLocaleSchema (Proxy :: Proxy LocaleDTO)
    , annotations = confirmationAnnotations {openWorldHint = True}
    , continuable = False
    , permissions = localeWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmationWhen "pull_registry_locale" arguments preparePull
    }
  where
    arguments =
      (,,,)
        <$> requiredArgument "code" "The code of the locale in the registry"
        <*> mailTemplatesArgument
        <*> optionalArgument "enabled" "Whether users can choose the locale"
        <*> optionalArgument "default_locale" "Make it the default locale of the organization"

preparePull :: WizardRequestContextC s m => (String, Bool, Maybe Bool, Maybe Bool) -> m (Either (m Value) (McpConfirmation m))
preparePull (code, False, mEnabled, mDefaultLocale) = prepareLocaleUpload (pullRegistryLocale code . RegistryLocalePullDTO) modifyLocaleFlags mEnabled mDefaultLocale
preparePull (code, True, mEnabled, mDefaultLocale) = prepareLocaleUpload (pullRegistryMailTemplateLocale code . RegistryLocalePullDTO) modifyMailTemplateLocaleFlags mEnabled mDefaultLocale

mailTemplatesArgument :: Arguments Bool
mailTemplatesArgument = fromMaybe False <$> optionalArgument "mail_templates" "The mail template locales instead of the UI locales"
