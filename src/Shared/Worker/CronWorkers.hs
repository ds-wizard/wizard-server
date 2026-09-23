module Shared.Worker.CronWorkers where

import Shared.Cache.CacheUtil
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Context.WizardServerContext
import Shared.Model.Worker.CronWorker
import Shared.Service.Document.DocumentCleanService
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService hiding (squash)
import Shared.Service.Project.Comment.ProjectCommentService
import Shared.Service.Project.Event.ProjectEventService hiding (squash)
import Shared.Service.Project.ProjectService
import Shared.Service.Registry.Synchronization.RegistrySynchronizationService
import Shared.Service.TemporaryFile.TemporaryFileService

workers :: (WizardServerContextType s, WizardRequestContextC r rm) => [CronWorker s rm]
workers =
  [ cacheWorker
  , documentWorker
  , squashKnowledgeModelEditorEventsWorker
  , cleanProjectWorker
  , squashProjectEventsWorker
  , assigneeNotificationWorker
  , registrySyncWorker
  , temporaryFileWorker
  ]

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
