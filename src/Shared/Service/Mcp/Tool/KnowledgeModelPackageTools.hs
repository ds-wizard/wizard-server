module Shared.Service.Mcp.Tool.KnowledgeModelPackageTools where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Api.Resource.Mcp.Registry.McpCatalogKindArgumentDTO
import Shared.Api.Resource.Mcp.Registry.McpCatalogKindArgumentJM ()
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileJM ()
import Shared.Api.Resource.Registry.RegistryCatalogItemDTO
import Shared.Api.Resource.Registry.RegistryCatalogItemJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Library.LibraryDependents
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Acl.LibraryAcl (checkDeleteAllowed)
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Registry.RegistryCatalogService
import Shared.Util.Error (tryError)
import Shared.Util.JsonSchema

knowledgeModelPackageTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelPackageTools =
  [ listKnowledgeModelPackages
  , getKnowledgeModelPackage
  , searchRegistryPackages
  , pullKnowledgeModelPackage
  , updateKnowledgeModelPackage
  , deleteKnowledgeModelPackage
  , exportKnowledgeModelPackage
  , importKnowledgeModelPackage
  ]

listKnowledgeModelPackages :: forall s m. WizardRequestContextC s m => McpTool m
listKnowledgeModelPackages =
  markThirdPartyFields [["_embedded", "knowledgeModelPackages", "name"], ["_embedded", "knowledgeModelPackages", "description"]] $
    McpTool
      { name = "list_knowledge_model_packages"
      , title = "List knowledge model packages"
      , description =
          "List the knowledge model packages (the latest version of each id), optionally in one workspace, of one id or only those with a newer version in the registry. "
            ++ "remoteLatestVersion is the latest version in the registry"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ pageSchema "knowledgeModelPackages" (jsonSchema (Proxy :: Proxy KnowledgeModelPackageSimpleDTO))
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ \(inScope, mId, mOutdated, (mQuery, pageable)) ->
          inScope (toJSON <$> getPackagesPage mId mQuery mOutdated pageable [Sort "name" Ascending])
      }
  where
    arguments =
      (,,,)
        <$> scopeArguments
        <*> optionalArgument "id" "Only the package with this id (without the version)"
        <*> optionalArgument "outdated" "Only the packages with (true) or without (false) a newer version in the registry"
        <*> searchArguments "Search in the names and ids of the packages"
        :: Arguments (m Value -> m Value, Maybe String, Maybe Bool, (Maybe String, Pageable))

getKnowledgeModelPackage :: WizardRequestContextC s m => McpTool m
getKnowledgeModelPackage =
  markThirdPartyFields [["package", "readme"]] $
    McpTool
      { name = "get_knowledge_model_package"
      , title = "Get knowledge model package"
      , description =
          "Get a knowledge model package with all its versions, its locales, its registry state and, when you may manage it, "
            ++ "its dependents (packages, editors, projects and documents based on it); check them before deprecating it"
      , inputSchema = argumentsSchema packageUuidArgument
      , outputSchema =
          Just $
            objectSchema
              ["package", "dependents"]
              [ ("package", jsonSchema (Proxy :: Proxy KnowledgeModelPackageDetailDTO))
              , ("dependents", describedSchema "Left out without knowledgeModels.manage" (jsonSchema (Proxy :: Proxy (Maybe LibraryDependents))))
              ]
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = NoTransaction
      , run = withArguments packageUuidArgument getPackageWithDependents
      }

getPackageWithDependents :: WizardRequestContextC s m => U.UUID -> m Value
getPackageWithDependents pkgUuid = do
  detail <- getPackageDetailByUuid pkgUuid False
  mDependents <- findManagedPackageDependents pkgUuid
  return $ object ["package" .= detail, "dependents" .= mDependents]

findManagedPackageDependents :: WizardRequestContextC s m => U.UUID -> m (Maybe LibraryDependents)
findManagedPackageDependents pkgUuid = do
  eDependents <- tryError (getPackageDependents pkgUuid Nothing)
  case eDependents of
    Right dependents -> return (Just dependents)
    Left (ForbiddenError _) -> return Nothing
    Left error -> throwError error

searchRegistryPackages :: WizardRequestContextC s m => McpTool m
searchRegistryPackages =
  markThirdPartyFields [["packages", "name"], ["packages", "description"]] $
    McpTool
      { name = "search_registry_packages"
      , title = "Search registry packages"
      , description =
          "Search the knowledge models and document templates of the registry by id, name or description and get the latest version of each. "
            ++ "It reads the catalog this instance synchronizes from the registry; pull a knowledge model with pull_knowledge_model_package"
      , inputSchema = argumentsSchema searchArguments'
      , outputSchema = Just $ objectSchema ["packages"] [("packages", arraySchema (jsonSchema (Proxy :: Proxy RegistryCatalogItemDTO)))]
      , annotations = readOnlyAnnotations {openWorldHint = True}
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = NoTransaction
      , run = withArguments searchArguments' $ \(mQuery, mKind) -> do
          items <- searchRegistryCatalog mQuery
          return $ object ["packages" .= filter (matchesKind mKind) items]
      }
  where
    searchArguments' =
      (,)
        <$> optionalArgument "q" "Search in the ids, names and descriptions"
        <*> optionalArgument "kind" "Only knowledge models or only document templates; both by default"

matchesKind :: Maybe McpCatalogKindArgumentDTO -> RegistryCatalogItemDTO -> Bool
matchesKind (Just (McpCatalogKindArgumentDTO kind)) item = item.kind == kind
matchesKind Nothing _ = True

pullKnowledgeModelPackage :: forall s m. WizardRequestContextC s m => McpTool m
pullKnowledgeModelPackage =
  McpTool
    { name = "pull_knowledge_model_package"
    , title = "Pull knowledge model package"
    , description =
        "Pull a knowledge model package from the registry into a workspace (or the organization with tenant: true) together with the packages it is based on. "
          ++ "id is the full id with the version, as search_registry_packages returns it plus \":\" and the version"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelPackageSimpleDTO)
    , annotations = idempotentWriteAnnotations {openWorldHint = True}
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, pkgId) -> inScope (toJSON <$> pullBundleFromRegistry pkgId)
    }
  where
    arguments = (,) <$> scopeArguments <*> requiredArgument "id" "The package to pull, e.g. dsw:root:2.7.0" :: Arguments (m Value -> m Value, String)

updateKnowledgeModelPackage :: WizardRequestContextC s m => McpTool m
updateKnowledgeModelPackage =
  McpTool
    { name = "update_knowledge_model_package"
    , title = "Update knowledge model package"
    , description =
        "Deprecate or release a knowledge model package version, or change whether it is public; what you leave out stays as it is. "
          ++ "Check its dependents with get_knowledge_model_package first"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelPackageSimpleDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(pkgUuid, mPhase, mPublic) -> toJSON <$> updatePackage pkgUuid mPhase mPublic
    }
  where
    arguments =
      (,,)
        <$> packageUuidArgument
        <*> optionalArgument "phase" "ReleasedKnowledgeModelPackagePhase or DeprecatedKnowledgeModelPackagePhase"
        <*> optionalArgument "public" "Whether the package is public"

updatePackage :: WizardRequestContextC s m => U.UUID -> Maybe KnowledgeModelPackagePhase -> Maybe Bool -> m KnowledgeModelPackageSimpleDTO
updatePackage pkgUuid mPhase mPublic = do
  pkg <- findPackageByUuid pkgUuid
  _ <- modifyPackage pkgUuid KnowledgeModelPackageChangeDTO {phase = fromMaybe pkg.phase mPhase, public = fromMaybe pkg.public mPublic}
  toSimpleDTO <$> findPackageByUuid pkgUuid

deleteKnowledgeModelPackage :: WizardRequestContextC s m => McpTool m
deleteKnowledgeModelPackage =
  McpTool
    { name = "delete_knowledge_model_package"
    , title = "Delete knowledge model package"
    , description =
        "Delete a knowledge model package version, or with all_versions every version of its id, together with the packages, editors, projects and documents based on it "
          ++ "(its dependents in get_knowledge_model_package). It cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments deletePackageArguments)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_knowledge_model_package" deletePackageArguments preparePackageDeletion
    }
  where
    deletePackageArguments = (,) <$> packageUuidArgument <*> allVersionsArgument

preparePackageDeletion :: WizardRequestContextC s m => (U.UUID, Bool) -> m (McpConfirmation m)
preparePackageDeletion (pkgUuid, allVersions) = do
  pkg <- findPackageByUuid pkgUuid
  dependents <- getPackageDependents pkgUuid (Just allVersions)
  checkDeleteAllowed dependents
  return
    McpConfirmation
      { summary = "Deletes " ++ deletedVersions allVersions "knowledge model package" pkg.id (show (createCoordinate pkg)) ++ packageDependentsSummary dependents ++ "; it cannot be undone"
      , revision = show dependents
      , perform = object [] <$ deletePackage pkgUuid (Just allVersions)
      }

packageDependentsSummary :: LibraryDependents -> String
packageDependentsSummary dependents =
  countsSummary
    " together with what is based on it: "
    [ (length dependents.knowledgeModelPackages, "knowledge model packages")
    , (length dependents.editors, "knowledge model editors")
    , (length dependents.projects, "projects")
    , (length dependents.documents, "documents")
    ]

exportKnowledgeModelPackage :: WizardRequestContextC s m => McpTool m
exportKnowledgeModelPackage =
  McpTool
    { name = "export_knowledge_model_package"
    , title = "Export knowledge model package"
    , description = "Export a knowledge model package with the packages it is based on as a .km bundle and get a temporary download url; import it elsewhere with import_knowledge_model_package"
    , inputSchema = argumentsSchema packageUuidArgument
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpTemporaryFileDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments packageUuidArgument (\uuid -> toJSON <$> mcpTemporaryFile (`getTemporaryFileWithBundle` uuid))
    }

importKnowledgeModelPackage :: forall s m. WizardRequestContextC s m => McpTool m
importKnowledgeModelPackage =
  McpTool
    { name = "import_knowledge_model_package"
    , title = "Import knowledge model package"
    , description = "Import a .km bundle (as export_knowledge_model_package or the registry produce it) into a workspace or the organization with tenant: true"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelPackageSimpleDTO)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, content) -> inScope (toJSON <$> importAndConvertBundle (BSL.fromStrict content) False)
    }
  where
    arguments =
      (,)
        <$> scopeArguments
        <*> requiredBase64ContentArgument "content" "The .km bundle, base64 encoded, at most 10 MB"
        :: Arguments (m Value -> m Value, BS.ByteString)

deletedVersions :: Bool -> String -> String -> String -> String
deletedVersions True entityName entityId _ = "all versions of the " ++ entityName ++ " " ++ entityId
deletedVersions False entityName _ coordinate = "the " ++ entityName ++ " " ++ coordinate

countsSummary :: String -> [(Int, String)] -> String
countsSummary prefix counts =
  case [show count ++ " " ++ entityName | (count, entityName) <- counts, count > 0] of
    [] -> ""
    present -> prefix ++ intercalate ", " present

allVersionsArgument :: Arguments Bool
allVersionsArgument = fromMaybe False <$> optionalArgument "all_versions" "Delete every version of the id, not only this one"

packageUuidArgument :: Arguments U.UUID
packageUuidArgument = requiredArgument "knowledge_model_package_uuid" "The knowledge model package"
