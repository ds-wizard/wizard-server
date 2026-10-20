module Shared.Service.Mcp.Tool.KnowledgeModelPublishTools where

import Data.Aeson (toJSON)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorPublication
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishService
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools (editorUuidArgument)
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema
import Shared.Util.List (lastSafe)

knowledgeModelPublishTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelPublishTools = [publishKnowledgeModelEditor]

publishKnowledgeModelEditor :: forall s m. WizardRequestContextC s m => McpTool m
publishKnowledgeModelEditor =
  McpTool
    { name = "publish_knowledge_model_editor"
    , title = "Publish knowledge model editor"
    , description =
        "Publish a knowledge model editor, or the completed migration of its fork, as a new knowledge model package. "
          ++ "The package gets the name, id, version, description and readme of the editor (change them with save_knowledge_model_editor first); "
          ++ "the version must be higher than the latest one of the id. Give locale_uuids to take over locales of the previous package. "
          ++ "Publishing cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ confirmationOutputSchema (jsonSchema (Proxy :: Proxy KnowledgeModelPackageSimpleDTO))
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "publish_knowledge_model_editor" arguments (uncurry preparePublish)
    }
  where
    arguments =
      (,)
        <$> editorUuidArgument
        <*> optionalArgument "locale_uuids" "Locales of the previous package to take over into the new one"
        :: Arguments (U.UUID, Maybe [U.UUID])

preparePublish :: WizardRequestContextC s m => U.UUID -> Maybe [U.UUID] -> m (McpConfirmation m)
preparePublish editorUuid mLocaleUuids = do
  publication <- getEditorPublication editorUuid mLocaleUuids
  return
    McpConfirmation
      { summary = publishSummary publication mLocaleUuids
      , revision = publishRevision publication
      , perform = toJSON <$> publish publication.editor publication.migration mLocaleUuids
      }

publishRevision :: KnowledgeModelEditorPublication -> String
publishRevision publication =
  intercalate
    "/"
    [ iso8601Show publication.editor.updatedAt
    , maybe "" (U.toString . (.uuid)) (lastSafe publication.events)
    , maybe "" (show . length . (.resultEvents)) publication.migration
    ]

publish :: WizardRequestContextC s m => KnowledgeModelEditor -> Maybe KnowledgeModelMigration -> Maybe [U.UUID] -> m KnowledgeModelPackageSimpleDTO
publish editor Nothing mLocaleUuids = publishPackageFromKnowledgeModelEditor (PackagePublishEditorDTO editor.uuid mLocaleUuids)
publish editor (Just _) mLocaleUuids = publishPackageFromMigration (PackagePublishMigrationDTO editor.uuid editor.version editor.description editor.readme mLocaleUuids)

publishSummary :: KnowledgeModelEditorPublication -> Maybe [U.UUID] -> String
publishSummary KnowledgeModelEditorPublication {..} mLocaleUuids =
  unwords
    [ "Publishes the knowledge model editor " ++ show editor.name ++ " as the package " ++ editor.id ++ ":" ++ editor.version
    , maybe ("with its " ++ show (length events) ++ " unpublished events.") (const "from the result of its completed migration.") migration
    , "Locales taken over: " ++ localeNames (filter ((`elem` fromMaybe [] mLocaleUuids) . (.uuid)) previousLocales) ++ "."
    , "Locales of the previous package: " ++ localeNames previousLocales ++ "."
    , "This cannot be undone."
    ]

localeNames :: [KnowledgeModelLocale] -> String
localeNames [] = "none"
localeNames locales = intercalate ", " [locale.name ++ " (" ++ locale.code ++ ", " ++ U.toString locale.uuid ++ ")" | locale <- locales]
