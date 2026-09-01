module Shared.Model.Config.WizardServerConfigDM where

import Shared.Model.Config.PublicServerConfigDM
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.WizardServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { general = defaultGeneral
    , database = defaultDatabase
    , s3 = defaultS3
    , aws = defaultAws
    , sentry = defaultSentry
    , userEmailLink = defaultUserEmailLink
    , cache = defaultCache
    , document = defaultDocument
    , externalLink = defaultExternalLink
    , knowledgeModelEditor = defaultKnowledgeModelEditor
    , project = defaultProject
    , temporaryFile = defaultTemporaryFile
    , userToken = defaultUserToken
    , userRegistration = defaultUserRegistration
    , analyticalMails = defaultAnalyticalMails
    , logging = defaultLogging
    , cloud = defaultCloud
    , persistentCommand = defaultPersistentCommand
    , signalBridge = defaultSignalBridge
    , admin = defaultAdmin
    , registry = defaultRegistry
    , httpClient = defaultHttpClient
    }

defaultHttpClient :: ServerConfigHttpClient
defaultHttpClient =
  ServerConfigHttpClient
    { restricted = ServerConfigHttpClientRestricted {allowedHosts = []}
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = "Production"
    , clientUrl = ""
    , serverPort = 3000
    , secret = ""
    , rsaPrivateKey = undefined
    , integrationConfig = "config/wizard/integration.yml"
    }

defaultRegistrySyncJob :: ServerConfigCronWorker
defaultRegistrySyncJob =
  ServerConfigCronWorker {enabled = True, cron = "*/15 * * * *"}

defaultUserEmailLink :: ServerConfigUserEmailLink
defaultUserEmailLink = ServerConfigUserEmailLink {clean = defaultUserEmailLinkClean}

defaultUserEmailLinkClean :: ServerConfigCronWorker
defaultUserEmailLinkClean =
  ServerConfigCronWorker {enabled = True, cron = "20 0 * * *"}

defaultUserRegistration :: ServerConfigUserRegistration
defaultUserRegistration = ServerConfigUserRegistration {clean = defaultUserRegistrationClean}

defaultUserRegistrationClean :: ServerConfigCronWorker
defaultUserRegistrationClean =
  ServerConfigCronWorker {enabled = True, cron = "30 0 * * *"}

defaultCache :: ServerConfigCache
defaultCache =
  ServerConfigCache
    { dataExpiration = 14 * 24
    , websocketExpiration = 24
    , purgeExpired = defaultCachePurgeExpired
    , dataEnabled = True
    }

defaultCachePurgeExpired :: ServerConfigCronWorker
defaultCachePurgeExpired =
  ServerConfigCronWorker {enabled = True, cron = "45 * * * *"}

defaultDocument :: ServerConfigDocument
defaultDocument = ServerConfigDocument {clean = defaultDocumentClean}

defaultDocumentClean :: ServerConfigCronWorker
defaultDocumentClean =
  ServerConfigCronWorker {enabled = True, cron = "0 */4 * * *"}

defaultKnowledgeModelEditor :: ServerConfigKnowledgeModelEditor
defaultKnowledgeModelEditor = ServerConfigKnowledgeModelEditor {squash = defaultKnowledgeModelEditorSquash}

defaultKnowledgeModelEditorSquash :: ServerConfigCronWorker
defaultKnowledgeModelEditorSquash =
  ServerConfigCronWorker {enabled = True, cron = "*/5 * * * *"}

defaultProject :: ServerConfigProject
defaultProject =
  ServerConfigProject
    { clean = defaultProjectClean
    , squash = defaultProjectSquash
    , assigneeNotification = defaultProjectAssigneeNotification
    }

defaultProjectClean :: ServerConfigCronWorker
defaultProjectClean =
  ServerConfigCronWorker {enabled = True, cron = "15 */4 * * *"}

defaultProjectSquash :: ServerConfigCronWorker
defaultProjectSquash =
  ServerConfigCronWorker {enabled = True, cron = "*/4 * * * *"}

defaultProjectAssigneeNotification :: ServerConfigCronWorker
defaultProjectAssigneeNotification =
  ServerConfigCronWorker {enabled = True, cron = "*/5 * * * *"}

defaultTemporaryFile :: ServerConfigTemporaryFile
defaultTemporaryFile = ServerConfigTemporaryFile {clean = defaultTemporaryFileClean}

defaultTemporaryFileClean :: ServerConfigCronWorker
defaultTemporaryFileClean =
  ServerConfigCronWorker {enabled = True, cron = "25 0 * * *"}

defaultUserToken :: ServerConfigUserToken
defaultUserToken = ServerConfigUserToken {clean = defaultUserTokenClean, expire = defaultUserTokenExpire}

defaultUserTokenClean :: ServerConfigCronWorker
defaultUserTokenClean =
  ServerConfigCronWorker {enabled = True, cron = "0 3 * * *"}

defaultUserTokenExpire :: ServerConfigCronWorker
defaultUserTokenExpire =
  ServerConfigCronWorker {enabled = True, cron = "0 4 * * *"}

defaultSignalBridge :: ServerConfigSignalBridge
defaultSignalBridge =
  ServerConfigSignalBridge
    { enabled = False
    , updatePermsArn = ""
    , updateUserGroupArn = ""
    , setProjectArn = ""
    , addEventArn = ""
    , addFileArn = ""
    , logOutAllArn = ""
    }

defaultAdmin :: ServerConfigAdmin
defaultAdmin =
  ServerConfigAdmin
    { enabled = False
    , serverUrl = ""
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { url = "https://api.registry.ds-wizard.org"
    , clientUrl = "https://registry.ds-wizard.org"
    , sync = defaultRegistrySyncJob
    }
