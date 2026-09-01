module Shared.Worker.CronWorkers where

import Control.Monad (void)

import Shared.Cache.CacheUtil
import Shared.Database.DAO.OpenId.OpenIdClientSessionDAO
import Shared.Database.VacuumCleaner
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Context.WizardServerContext
import Shared.Model.Worker.CronWorker
import Shared.Service.Document.DocumentCleanService
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService hiding (squash)
import qualified Shared.Service.PersistentCommand.PersistentCommandExecutor as PersistentCommandExecutor
import Shared.Service.PersistentCommand.PersistentCommandService
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Service.Project.Comment.ProjectCommentService
import Shared.Service.Project.Event.ProjectEventService hiding (squash)
import Shared.Service.Project.ProjectService
import Shared.Service.Registry.Synchronization.RegistrySynchronizationService
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.User.RegistrationPending.UserRegistrationPendingService
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.ApiKey.ApiKeyService
import Shared.Service.UserToken.UserTokenService

workers :: (WizardServerContextType s, WizardRequestContextC r rm) => [CronWorker s rm]
workers =
  [ userEmailLinkWorker
  , cacheWorker
  , documentWorker
  , squashKnowledgeModelEditorEventsWorker
  , persistentCommandRetryWorker
  , persistentCommandRetryLambdaWorker
  , cleanProjectWorker
  , squashProjectEventsWorker
  , assigneeNotificationWorker
  , registrySyncWorker
  , temporaryFileWorker
  , cleanUserRegistrationPendingWorker
  , cleanUserTokenWorker
  , expireUserTokenWorker
  , vacuumCleanerWorker
  , cleanOpenIdClientSessionWorker
  ]

-- ------------------------------------------------------------------
cleanOpenIdClientSessionWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanOpenIdClientSessionWorker =
  CronWorker
    { name = "CleanOpenIdClientSessionWorker"
    , condition = \c -> not c.serverConfig'.admin.enabled
    , cron = const "*/30 * * * *"
    , function = void deleteExpiredOpenIdClientSessions
    , wrapInTransaction = True
    }

-- ------------------------------------------------------------------
userEmailLinkWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
userEmailLinkWorker =
  CronWorker
    { name = "UserEmailLinkWorker"
    , condition = \c -> c.serverConfig'.userEmailLink.clean.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.userEmailLink.clean.cron)
    , function = cleanUserEmailLinks
    , wrapInTransaction = True
    }

cacheWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cacheWorker =
  CronWorker
    { name = "CacheWorker"
    , condition = (.serverConfig'.cache.purgeExpired.enabled)
    , cron = (.serverConfig'.cache.purgeExpired.cron)
    , function = purgeExpiredCache
    , wrapInTransaction = True
    }

documentWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
documentWorker =
  CronWorker
    { name = "DocumentWorker"
    , condition = (.serverConfig'.project.clean.enabled)
    , cron = (.serverConfig'.project.clean.cron)
    , function = cleanDocuments
    , wrapInTransaction = True
    }

squashKnowledgeModelEditorEventsWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
squashKnowledgeModelEditorEventsWorker =
  CronWorker
    { name = "SquashKnowledgeModelEditorEventsWorker"
    , condition = (.serverConfig'.knowledgeModelEditor.squash.enabled)
    , cron = (.serverConfig'.knowledgeModelEditor.squash.cron)
    , function = squashEvents
    , wrapInTransaction = True
    }

persistentCommandRetryWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
persistentCommandRetryWorker =
  CronWorker
    { name = "PersistentCommandRetryWorker"
    , condition = (.serverConfig'.persistentCommand.retryJob.enabled)
    , cron = (.serverConfig'.persistentCommand.retryJob.cron)
    , function = runPersistentCommands'
    , wrapInTransaction = True
    }

persistentCommandRetryLambdaWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
persistentCommandRetryLambdaWorker =
  CronWorker
    { name = "persistentCommandRetryLambdaWorker"
    , condition = \c -> c.serverConfig'.persistentCommand.retryLambdaJob.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.persistentCommand.retryLambdaJob.cron)
    , function = retryPersistentCommandsForLambda PersistentCommandExecutor.components
    , wrapInTransaction = False
    }

cleanProjectWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanProjectWorker =
  CronWorker
    { name = "CleanProjectWorker"
    , condition = (.serverConfig'.project.clean.enabled)
    , cron = (.serverConfig'.project.clean.cron)
    , function = cleanProjects
    , wrapInTransaction = True
    }

squashProjectEventsWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
squashProjectEventsWorker =
  CronWorker
    { name = "SquashProjectEventsWorker"
    , condition = (.serverConfig'.project.squash.enabled)
    , cron = (.serverConfig'.project.squash.cron)
    , function = squashProjectEvents
    , wrapInTransaction = True
    }

assigneeNotificationWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
assigneeNotificationWorker =
  CronWorker
    { name = "AssigneeNotificationWorker"
    , condition = (.serverConfig'.project.assigneeNotification.enabled)
    , cron = (.serverConfig'.project.assigneeNotification.cron)
    , function = sendNotificationToNewAssignees
    , wrapInTransaction = True
    }

registrySyncWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
registrySyncWorker =
  CronWorker
    { name = "RegistryWorker"
    , condition = (.serverConfig'.registry.sync.enabled)
    , cron = (.serverConfig'.registry.sync.cron)
    , function = synchronizeData
    , wrapInTransaction = True
    }

temporaryFileWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
temporaryFileWorker =
  CronWorker
    { name = "TemporaryFileWorker"
    , condition = (.serverConfig'.temporaryFile.clean.enabled)
    , cron = (.serverConfig'.temporaryFile.clean.cron)
    , function = cleanTemporaryFiles
    , wrapInTransaction = True
    }

cleanUserTokenWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanUserTokenWorker =
  CronWorker
    { name = "CleanUserTokenWorker"
    , condition = \c -> c.serverConfig'.userToken.clean.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.userToken.clean.cron)
    , function = cleanTokens
    , wrapInTransaction = True
    }

cleanUserRegistrationPendingWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanUserRegistrationPendingWorker =
  CronWorker
    { name = "CleanUserRegistrationPendingWorker"
    , condition = \c -> c.serverConfig'.userRegistration.clean.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.userRegistration.clean.cron)
    , function = cleanUserRegistrationPending
    , wrapInTransaction = True
    }

expireUserTokenWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
expireUserTokenWorker =
  CronWorker
    { name = "ExpireUserTokenWorker"
    , condition = \c -> c.serverConfig'.userToken.expire.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.userToken.expire.cron)
    , function = expireApiKeys
    , wrapInTransaction = True
    }

vacuumCleanerWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
vacuumCleanerWorker =
  CronWorker
    { name = "VacuumCleanerWorker"
    , condition = \c -> c.serverConfig'.database.vacuumCleaner.enabled && not c.serverConfig'.admin.enabled
    , cron = (.serverConfig'.database.vacuumCleaner.cron)
    , function = runVacuumCleaner
    , wrapInTransaction = False
    }
