module Shared.Service.Mcp.Tool.MailTemplateLocaleTools where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService
import Shared.Service.Mcp.Tool.LocaleTools
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

mailTemplateLocaleTools :: WizardRequestContextC s m => [McpTool m]
mailTemplateLocaleTools = [listMailTemplateLocales, getMailTemplateTranslation, saveMailTemplateLocale, deleteMailTemplateLocaleTool]

listMailTemplateLocales :: WizardRequestContextC s m => McpTool m
listMailTemplateLocales =
  McpTool
    { name = "list_mail_template_locales"
    , title = "List mail template locales"
    , description =
        "List the locales of the e-mail templates, with whether each is enabled and which one is the default, "
          ++ "and the translation template they are made from (pot: default or the organization's custom templates, and its version)"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ withProperties [("pot", jsonSchema (Proxy :: Proxy MailTemplatePotInfoDTO))] (localesSchema (Proxy :: Proxy MailTemplateLocaleDTO))
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = localeReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\locales pot -> object ["locales" .= locales, "pot" .= pot]) <$> getMailTemplateLocales <*> getMailTemplatePotInfo
    }

getMailTemplateTranslation :: WizardRequestContextC s m => McpTool m
getMailTemplateTranslation =
  markThirdPartyFields [["content"]] $
    McpTool
      { name = "get_mail_template_translation"
      , title = "Get mail template translation"
      , description =
          "Get the translation template (POT) of the e-mail templates, or with locale_uuid the PO file of a mail template locale; "
            ++ "translate or edit it and save it with save_mail_template_locale. Long content is cut, call it again with the cursor of the result to read on"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just translationSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = localeReadPermissions
      , toolset = Just AdminMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ fmap translationResult . getMailTemplateTranslationContent
      }
  where
    arguments = optionalArgument "locale_uuid" "The locale whose PO file to get; leave it out for the translation template"

getMailTemplateTranslationContent :: WizardRequestContextC s m => Maybe U.UUID -> m BS.ByteString
getMailTemplateTranslationContent = maybe getMailTemplatePotContent getMailTemplateLocalePoContent

saveMailTemplateLocale :: WizardRequestContextC s m => McpTool m
saveMailTemplateLocale =
  McpTool
    { name = "save_mail_template_locale"
    , title = "Save mail template locale"
    , description = saveLocaleDescription "mail template locale" "list_mail_template_locales"
    , inputSchema = argumentsSchema (confirmationArguments saveLocaleArguments)
    , outputSchema = Just $ saveLocaleSchema (Proxy :: Proxy MailTemplateLocaleDTO)
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = localeWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmationWhen "save_mail_template_locale" saveLocaleArguments (prepareLocaleSave uploadMailTemplateLocale modifyMailTemplateLocaleFlags)
    }

deleteMailTemplateLocaleTool :: WizardRequestContextC s m => McpTool m
deleteMailTemplateLocaleTool =
  McpTool
    { name = "delete_mail_template_locale"
    , title = "Delete mail template locale"
    , description = "Delete a mail template locale; e-mails in its language are sent in the default locale instead"
    , inputSchema = argumentsSchema localeUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = localeWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments localeUuidArgument $ \uuid -> object [] <$ deleteMailTemplateLocale uuid
    }
