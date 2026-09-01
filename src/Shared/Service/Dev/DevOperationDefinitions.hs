module Shared.Service.Dev.DevOperationDefinitions where

import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)

import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Cache.CacheUtil
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.ContextMappers
import Shared.Model.Context.WizardRequestContext hiding (cache)
import Shared.Model.Dev.Dev
import Shared.Model.Tenant.Tenant
import Shared.Service.Document.DocumentCleanService
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService
import Shared.Service.KnowledgeModel.Metamodel.MigrationService
import Shared.Service.Owl.OwlService
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Service.Plugin.PluginService
import Shared.Service.Project.Comment.ProjectCommentService
import Shared.Service.Project.Event.ProjectEventService
import Shared.Service.Project.ProjectService
import Shared.Service.Registry.Push.RegistryPushService
import Shared.Service.Registry.Synchronization.RegistrySynchronizationService
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.User.WizardUserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.ApiKey.ApiKeyService
import Shared.Service.UserToken.UserTokenService
import Shared.Util.Uuid

sections :: WizardRequestContextC s m => [DevSection m]
sections =
  [ apiKey
  , cache
  , document
  , knowledgeModelEditor
  , metamodelMigrator
  , owl
  , persistentCommand
  , plugin
  , project
  , registry
  , temporaryFile
  , user
  , userEmailLink
  ]

-- ---------------------------------------------------------------------------------------------------------------------
-- API KEY
-- ---------------------------------------------------------------------------------------------------------------------
apiKey :: WizardRequestContextC s m => DevSection m
apiKey =
  DevSection
    { name = "API Key"
    , description = Nothing
    , operations = [apiKey_expireApiKeys]
    }

-- ---------------------------------------------------------------------------------------------------------------------
apiKey_expireApiKeys :: WizardRequestContextC s m => DevOperation m
apiKey_expireApiKeys =
  DevOperation
    { name = "Send Api Key Expiration Mails"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        expireApiKeys
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- KNOWLEDGE MODEL EDITOR
-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelEditor :: WizardRequestContextC s m => DevSection m
knowledgeModelEditor =
  DevSection
    { name = "Knowledge Model Editor"
    , description = Nothing
    , operations =
        [ knowledgeModelEditor_squashAllEvents
        , knowledgeModelEditor_squashEventsForEditor
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelEditor_squashAllEvents :: WizardRequestContextC s m => DevOperation m
knowledgeModelEditor_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        squashEvents
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelEditor_squashEventsForEditor :: WizardRequestContextC s m => DevOperation m
knowledgeModelEditor_squashEventsForEditor =
  DevOperation
    { name = "Squash Events for Knowledge Model Editor"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "editorUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        squashEventsForEditor (u' . head $ reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- CACHE
-- ---------------------------------------------------------------------------------------------------------------------
cache :: WizardRequestContextC s m => DevSection m
cache =
  DevSection
    { name = "Cache"
    , description = Nothing
    , operations =
        [ cache_purgeCache
        , cache_getUserTokenCacheSize
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_purgeCache :: WizardRequestContextC s m => DevOperation m
cache_purgeCache =
  DevOperation
    { name = "Purge All Caches"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        purgeCache
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_getUserTokenCacheSize :: WizardRequestContextC s m => DevOperation m
cache_getUserTokenCacheSize =
  DevOperation
    { name = "Get User Token Cache Size"
    , description = Nothing
    , parameters = []
    , function = const computeUserTokenCacheSize
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- DOCUMENT
-- ---------------------------------------------------------------------------------------------------------------------
document :: WizardRequestContextC s m => DevSection m
document =
  DevSection
    { name = "Document"
    , description = Nothing
    , operations = [document_cleanDocuments]
    }

-- ---------------------------------------------------------------------------------------------------------------------
document_cleanDocuments :: WizardRequestContextC s m => DevOperation m
document_cleanDocuments =
  DevOperation
    { name = "Clean Expired Documents"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanDocuments
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- METAMODEL MIGRATOR
-- ---------------------------------------------------------------------------------------------------------------------
metamodelMigrator :: WizardRequestContextC s m => DevSection m
metamodelMigrator =
  DevSection
    { name = "Metamodel Migrator"
    , description = Nothing
    , operations = [metamodelMigrator_migrate]
    }

-- ---------------------------------------------------------------------------------------------------------------------
metamodelMigrator_migrate :: WizardRequestContextC s m => DevOperation m
metamodelMigrator_migrate =
  DevOperation
    { name = "Migrate"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "tenantUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        let tenantUuid = u' . head $ reqDto.parameters
        tenant <- findTenantByUuid tenantUuid
        migrateToLatestMetamodelVersionCommand tenant Nothing
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- OWL
-- ---------------------------------------------------------------------------------------------------------------------
owl :: WizardRequestContextC s m => DevSection m
owl =
  DevSection
    { name = "Owl"
    , description = Nothing
    , operations =
        [ owl_switchOwlOn
        , owl_switchOwlOff
        , owl_setOwlProperties
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_switchOwlOn :: WizardRequestContextC s m => DevOperation m
owl_switchOwlOn =
  DevOperation
    { name = "Enable OWL feature"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        modifyOwlFeature True
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_switchOwlOff :: WizardRequestContextC s m => DevOperation m
owl_switchOwlOff =
  DevOperation
    { name = "Disable OWL feature"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        modifyOwlFeature False
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_setOwlProperties :: WizardRequestContextC s m => DevOperation m
owl_setOwlProperties =
  DevOperation
    { name = "Set OWL properties"
    , description = Just "If you do not want to fill `previousPackageUuid`, please fill empty space (`' '`)"
    , parameters =
        [ DevOperationParameter
            { name = "name"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "organizationId"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "kmId"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "version"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "previousPackageUuid"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "rootElement"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        let name = head reqDto.parameters
        let organizationId = reqDto.parameters !! 1
        let kmId = reqDto.parameters !! 2
        let version = reqDto.parameters !! 3
        let previousPackageUuid =
              case reqDto.parameters !! 4 of
                " " -> Nothing
                p -> Just p
        let rootElement = reqDto.parameters !! 5
        setOwlProperties name organizationId kmId version previousPackageUuid rootElement
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- PERSISTENT COMMAND
-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand :: WizardRequestContextC s m => DevSection m
persistentCommand =
  DevSection
    { name = "Persistent Command"
    , description = Nothing
    , operations = [persistentCommand_runAll, persistentCommand_run]
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_runAll :: forall s m. WizardRequestContextC s m => DevOperation m
persistentCommand_runAll =
  DevOperation
    { name = "Run All Persistent Commands"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        context <- ask
        tenants <- findTenants
        let tenantUuids = fmap (.uuid) tenants
        liftIO $ traverse_ (\tenantUuid -> runWithConnection @s @m runPersistentCommands' (setCurrentUser (Just . toDTO $ userSystem) (setTenantUuid tenantUuid context))) tenantUuids
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_run :: WizardRequestContextC s m => DevOperation m
persistentCommand_run =
  DevOperation
    { name = "Run Persistent Command"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        command <- findPersistentCommandSimpleByUuid (u' . head $ reqDto.parameters)
        runPersistentCommand' True command
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- PLUGIN
-- ---------------------------------------------------------------------------------------------------------------------
plugin :: WizardRequestContextC s m => DevSection m
plugin =
  DevSection
    { name = "Plugin"
    , description = Nothing
    , operations =
        [ plugin_addAll
        , plugin_addForTenant
        , plugin_updateAll
        , plugin_updateForTenant
        , plugin_deleteAll
        , plugin_deleteForTenant
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_addAll :: WizardRequestContextC s m => DevOperation m
plugin_addAll =
  DevOperation
    { name = "Add Plugin for All Tenants"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "url"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "enabled"
            , aType = BoolDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        createPluginForAllTenants (u' . head $ reqDto.parameters) (reqDto.parameters !! 1) (read $ reqDto.parameters !! 2)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_addForTenant :: WizardRequestContextC s m => DevOperation m
plugin_addForTenant =
  DevOperation
    { name = "Add Plugin for Tenant"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "tenantUuid"
            , aType = TenantDevOperationParameterType
            }
        , DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "url"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "enabled"
            , aType = BoolDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        createPluginForTenant (u' . head $ reqDto.parameters) (u' $ reqDto.parameters !! 1) (reqDto.parameters !! 2) (read $ reqDto.parameters !! 3)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_updateAll :: WizardRequestContextC s m => DevOperation m
plugin_updateAll =
  DevOperation
    { name = "Update Plugin for All Tenants"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "url"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        updatePluginUrlForAllTenants (u' . head $ reqDto.parameters) (reqDto.parameters !! 1)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_updateForTenant :: WizardRequestContextC s m => DevOperation m
plugin_updateForTenant =
  DevOperation
    { name = "Update Plugin for Tenant"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "tenantUuid"
            , aType = TenantDevOperationParameterType
            }
        , DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "url"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        updatePluginUrlForTenant (u' . head $ reqDto.parameters) (u' $ reqDto.parameters !! 1) (reqDto.parameters !! 2)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_deleteAll :: WizardRequestContextC s m => DevOperation m
plugin_deleteAll =
  DevOperation
    { name = "Delete Plugin for All Tenants"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        deletePluginForAllTenants (u' . head $ reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
plugin_deleteForTenant :: WizardRequestContextC s m => DevOperation m
plugin_deleteForTenant =
  DevOperation
    { name = "Delete Plugin for Tenant"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "tenantUuid"
            , aType = TenantDevOperationParameterType
            }
        , DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        deletePluginForTenant (u' . head $ reqDto.parameters) (u' $ reqDto.parameters !! 1)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- PROJECT
-- ---------------------------------------------------------------------------------------------------------------------
project :: WizardRequestContextC s m => DevSection m
project =
  DevSection
    { name = "Project"
    , description = Nothing
    , operations =
        [ project_cleanProjects
        , project_squashAllEvents
        , project_squashEventsForProject
        , project_sendNotificationToNewAssignees
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
project_cleanProjects :: WizardRequestContextC s m => DevOperation m
project_cleanProjects =
  DevOperation
    { name = "Clean Projects"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanProjects
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
project_squashAllEvents :: WizardRequestContextC s m => DevOperation m
project_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        squashProjectEvents
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
project_squashEventsForProject :: WizardRequestContextC s m => DevOperation m
project_squashEventsForProject =
  DevOperation
    { name = "Squash Events for Project"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "projectUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        squashProjectEventsForProject (u' . head $ reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
project_sendNotificationToNewAssignees :: WizardRequestContextC s m => DevOperation m
project_sendNotificationToNewAssignees =
  DevOperation
    { name = "Send Notification to New Assignees"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        sendNotificationToNewAssignees
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- REGISTRY
-- ---------------------------------------------------------------------------------------------------------------------
registry :: WizardRequestContextC s m => DevSection m
registry =
  DevSection
    { name = "Registry"
    , description = Nothing
    , operations = [registry_syncWithRegistry, registry_pushKnowledgeModelBundle, registry_pushDocumentTemplateBundle, registry_pushLocaleBundle]
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_syncWithRegistry :: WizardRequestContextC s m => DevOperation m
registry_syncWithRegistry =
  DevOperation
    { name = "Sync with registry"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        synchronizeData
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushKnowledgeModelBundle :: WizardRequestContextC s m => DevOperation m
registry_pushKnowledgeModelBundle =
  DevOperation
    { name = "Push Knowledge model Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        pushKnowledgeModelBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushDocumentTemplateBundle :: WizardRequestContextC s m => DevOperation m
registry_pushDocumentTemplateBundle =
  DevOperation
    { name = "Push Document Template Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        pushDocumentTemplateBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushLocaleBundle :: WizardRequestContextC s m => DevOperation m
registry_pushLocaleBundle =
  DevOperation
    { name = "Push Locale Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        pushLocaleBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- TEMPORARY FILE
-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile :: WizardRequestContextC s m => DevSection m
temporaryFile =
  DevSection
    { name = "Temporary File"
    , description = Nothing
    , operations = [temporaryFile_cleanTemporaryFiles]
    }

-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile_cleanTemporaryFiles :: WizardRequestContextC s m => DevOperation m
temporaryFile_cleanTemporaryFiles =
  DevOperation
    { name = "Clean Expired Temporary Files"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanTemporaryFiles
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- USER
-- ---------------------------------------------------------------------------------------------------------------------
user :: WizardRequestContextC s m => DevSection m
user =
  DevSection
    { name = "User"
    , description = Nothing
    , operations = [user_cleanTokens]
    }

-- ---------------------------------------------------------------------------------------------------------------------
user_cleanTokens :: WizardRequestContextC s m => DevOperation m
user_cleanTokens =
  DevOperation
    { name = "Clean Expired Tokens"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanTokens
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- USER EMAIL LINK
-- ---------------------------------------------------------------------------------------------------------------------
userEmailLink :: WizardRequestContextC s m => DevSection m
userEmailLink =
  DevSection
    { name = "User Email Link"
    , description = Nothing
    , operations = [userEmailLink_cleanUserEmailLinks]
    }

-- ---------------------------------------------------------------------------------------------------------------------
userEmailLink_cleanUserEmailLinks :: WizardRequestContextC s m => DevOperation m
userEmailLink_cleanUserEmailLinks =
  DevOperation
    { name = "Clean Expired User Email Links"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanUserEmailLinks
        return "Done"
    }
