module WizardServer.Worker.CronWorkers where

import Control.Monad (void)

import Shared.Database.DAO.OpenId.OpenIdClientSessionDAO
import Shared.Database.VacuumCleaner
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Context.WizardServerContext
import Shared.Model.Worker.CronWorker
import qualified Shared.Service.PersistentCommand.PersistentCommandExecutor as PersistentCommandExecutor
import Shared.Service.PersistentCommand.PersistentCommandService
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Service.User.RegistrationPending.UserRegistrationPendingService
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.ApiKey.ApiKeyService
import Shared.Service.UserToken.UserTokenService
import qualified Shared.Worker.CronWorkers as Shared

workers :: (WizardServerContextType s, WizardRequestContextC r rm) => [CronWorker s rm]
workers =
  Shared.workers
    ++ [ userEmailLinkWorker
       , persistentCommandRetryWorker
       , persistentCommandRetryLambdaWorker
       , cleanUserTokenWorker
       , expireUserTokenWorker
       , cleanUserRegistrationPendingWorker
       , vacuumCleanerWorker
       , cleanOpenIdClientSessionWorker
       ]

userEmailLinkWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
userEmailLinkWorker =
  CronWorker
    { name = "UserEmailLinkWorker"
    , condition = (.serverConfig'.userEmailLink.clean.enabled)
    , cron = (.serverConfig'.userEmailLink.clean.cron)
    , function = cleanUserEmailLinks
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
    , condition = (.serverConfig'.persistentCommand.retryLambdaJob.enabled)
    , cron = (.serverConfig'.persistentCommand.retryLambdaJob.cron)
    , function = retryPersistentCommandsForLambda PersistentCommandExecutor.components
    , wrapInTransaction = False
    }

cleanUserTokenWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanUserTokenWorker =
  CronWorker
    { name = "CleanUserTokenWorker"
    , condition = (.serverConfig'.userToken.clean.enabled)
    , cron = (.serverConfig'.userToken.clean.cron)
    , function = cleanTokens
    , wrapInTransaction = True
    }

expireUserTokenWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
expireUserTokenWorker =
  CronWorker
    { name = "ExpireUserTokenWorker"
    , condition = (.serverConfig'.userToken.expire.enabled)
    , cron = (.serverConfig'.userToken.expire.cron)
    , function = expireApiKeys
    , wrapInTransaction = True
    }

cleanUserRegistrationPendingWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanUserRegistrationPendingWorker =
  CronWorker
    { name = "CleanUserRegistrationPendingWorker"
    , condition = (.serverConfig'.userRegistration.clean.enabled)
    , cron = (.serverConfig'.userRegistration.clean.cron)
    , function = cleanUserRegistrationPending
    , wrapInTransaction = True
    }

vacuumCleanerWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
vacuumCleanerWorker =
  CronWorker
    { name = "VacuumCleanerWorker"
    , condition = (.serverConfig'.database.vacuumCleaner.enabled)
    , cron = (.serverConfig'.database.vacuumCleaner.cron)
    , function = runVacuumCleaner
    , wrapInTransaction = False
    }

cleanOpenIdClientSessionWorker :: (WizardServerContextType s, WizardRequestContextC r rm) => CronWorker s rm
cleanOpenIdClientSessionWorker =
  CronWorker
    { name = "CleanOpenIdClientSessionWorker"
    , condition = const True
    , cron = const "*/30 * * * *"
    , function = void deleteExpiredOpenIdClientSessions
    , wrapInTransaction = True
    }

-- ------------------------------------------------------------------
