module Shared.Service.Mcp.Tool.KnowledgeModelLocaleTools where

import Data.Aeson
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Constant.Mcp
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService
import Shared.Service.KnowledgeModel.Locale.Pot.PotFileService
import Shared.Service.Mcp.Tool.KnowledgeModelPackageTools (packageUuidArgument)
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

knowledgeModelLocaleTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelLocaleTools = [getKnowledgeModelTranslation, saveKnowledgeModelLocale, deleteKnowledgeModelLocale]

getKnowledgeModelTranslation :: WizardRequestContextC s m => McpTool m
getKnowledgeModelTranslation =
  markThirdPartyFields [["content"]] $
    McpTool
      { name = "get_knowledge_model_translation"
      , title = "Get knowledge model translation"
      , description =
          "Get the translation template (POT) of a knowledge model package, or with locale_uuid the PO file of one of its locales (listed in get_knowledge_model_package). "
            ++ "Translate the template and save it with save_knowledge_model_locale; long content is cut, call it again with the cursor of the result to read on"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ objectSchema ["content"] [("content", stringSchema)]
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(pkgUuid, mLocaleUuid) -> (\content -> object ["content" .= content]) <$> getTranslation pkgUuid mLocaleUuid
      }
  where
    arguments = (,) <$> packageUuidArgument <*> optionalArgument "locale_uuid" "The locale whose PO file to get; leave it out for the translation template"

getTranslation :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m String
getTranslation pkgUuid Nothing = snd <$> getTranslationTemplate pkgUuid
getTranslation pkgUuid (Just localeUuid) = decodeText <$> getLocaleContent pkgUuid localeUuid

saveKnowledgeModelLocale :: WizardRequestContextC s m => McpTool m
saveKnowledgeModelLocale =
  McpTool
    { name = "save_knowledge_model_locale"
    , title = "Save knowledge model locale"
    , description =
        "Add a translation (a PO file translated from get_knowledge_model_translation) to a knowledge model package; its Language header gives the locale code "
          ++ "and a locale of the same code is replaced. name defaults to the X-Display-Name header or the code"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelLocaleList)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(pkgUuid, mName, content) -> toJSON <$> saveLocaleFromPo pkgUuid mName content
    }
  where
    arguments =
      (,,)
        <$> packageUuidArgument
        <*> optionalArgument "name" "The name of the locale"
        <*> requiredTextContentArgument mcpMaxBase64ContentSize "content" "The PO file, at most 10 MB"

deleteKnowledgeModelLocale :: WizardRequestContextC s m => McpTool m
deleteKnowledgeModelLocale =
  McpTool
    { name = "delete_knowledge_model_locale"
    , title = "Delete knowledge model locale"
    , description = "Delete a locale of a knowledge model package; projects in its language fall back to the original texts"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(pkgUuid, localeUuid) -> object [] <$ deleteLocale pkgUuid localeUuid
    }
  where
    arguments = (,) <$> packageUuidArgument <*> requiredArgument "locale_uuid" "The locale to delete"
