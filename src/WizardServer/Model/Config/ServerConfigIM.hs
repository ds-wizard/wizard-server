module WizardServer.Model.Config.ServerConfigIM where

import Shared.Model.Config.PublicServerConfigIM ()
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.ServerConfigIM
import Shared.Model.Config.WizardServerConfig

instance FromEnv ServerConfig where
  applyEnv serverConfig = do
    general <- applyEnv serverConfig.general
    database <- applyEnv serverConfig.database
    s3 <- applyEnv serverConfig.s3
    aws <- applyEnv serverConfig.aws
    sentry <- applyEnv serverConfig.sentry
    userEmailLink <- applyEnv serverConfig.userEmailLink
    auditLog <- applyEnv serverConfig.auditLog
    cache <- applyEnv serverConfig.cache
    document <- applyEnv serverConfig.document
    externalLink <- applyEnv serverConfig.externalLink
    knowledgeModelEditor <- applyEnv serverConfig.knowledgeModelEditor
    project <- applyEnv serverConfig.project
    temporaryFile <- applyEnv serverConfig.temporaryFile
    websocket <- applyEnv serverConfig.websocket
    userToken <- applyEnv serverConfig.userToken
    userRegistration <- applyEnv serverConfig.userRegistration
    logging <- applyEnv serverConfig.logging
    multiTenancy <- applyEnv serverConfig.multiTenancy
    persistentCommand <- applyEnv serverConfig.persistentCommand
    admin <- applyEnv serverConfig.admin
    registry <- applyEnv serverConfig.registry
    httpClient <- applyEnv serverConfig.httpClient
    mcp <- applyEnv serverConfig.mcp
    return ServerConfig {..}

instance FromEnv ServerConfigMcp where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "MCP_DOCUMENT_PREVIEW_WAIT" c.documentPreviewWait (\x -> c {documentPreviewWait = x})
      , \c -> applyEnvVariable "MCP_DOCUMENT_PREVIEW_LIMIT" c.documentPreviewLimit (\x -> c {documentPreviewLimit = x})
      ]

instance FromEnv ServerConfigHttpClient where
  applyEnv serverConfig = do
    restricted <- applyEnv serverConfig.restricted
    return ServerConfigHttpClient {..}

instance FromEnv ServerConfigHttpClientRestricted where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "HTTP_CLIENT_RESTRICTED_ALLOWED_HOSTS" c.allowedHosts (\x -> c {allowedHosts = x} :: ServerConfigHttpClientRestricted)
      ]

instance FromEnv ServerConfigGeneral where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "GENERAL_ENVIRONMENT" c.environment (\x -> c {environment = x} :: ServerConfigGeneral)
      , \c -> applyStringEnvVariable "GENERAL_CLIENT_URL" c.clientUrl (\x -> c {clientUrl = x} :: ServerConfigGeneral)
      , \c -> applyEnvVariable "GENERAL_SERVER_PORT" c.serverPort (\x -> c {serverPort = x})
      , \c -> applyStringEnvVariable "GENERAL_SECRET" c.secret (\x -> c {secret = x})
      , \c -> applyRSAPrivateKeyEnvVariable "GENERAL_RSA_PRIVATE_KEY" c.rsaPrivateKey (\x -> c {rsaPrivateKey = x} :: ServerConfigGeneral)
      , \c -> applyStringEnvVariable "GENERAL_INTEGRATION_CONFIG" c.integrationConfig (\x -> c {integrationConfig = x})
      , \c -> applyMaybeStringEnvVariable "GENERAL_PRIVACY_URL" c.privacyUrl (\x -> c {privacyUrl = x} :: ServerConfigGeneral)
      , \c -> applyMaybeStringEnvVariable "GENERAL_TERMS_OF_SERVICE_URL" c.termsOfServiceUrl (\x -> c {termsOfServiceUrl = x} :: ServerConfigGeneral)
      ]

instance FromEnv ServerConfigUserRegistration where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "USER_REGISTRATION_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigUserRegistration)
      , \c -> applyStringEnvVariable "USER_REGISTRATION_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigUserRegistration)
      ]

instance FromEnv ServerConfigCache where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "CACHE_DATA_EXPIRATION" c.dataExpiration (\x -> c {dataExpiration = x})
      , \c -> applyEnvVariable "CACHE_PURGE_EXPIRED_ENABLED" c.purgeExpired.enabled (\x -> c {purgeExpired = c.purgeExpired {enabled = x}} :: ServerConfigCache)
      , \c -> applyStringEnvVariable "CACHE_PURGE_EXPIRED_CRON" c.purgeExpired.cron (\x -> c {purgeExpired = c.purgeExpired {cron = x}} :: ServerConfigCache)
      , \c -> applyEnvVariable "CACHE_DATA_ENABLED" c.dataEnabled (\x -> c {dataEnabled = x} :: ServerConfigCache)
      ]

instance FromEnv ServerConfigDocument where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "DOCUMENT_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigDocument)
      , \c -> applyStringEnvVariable "DOCUMENT_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigDocument)
      ]

instance FromEnv ServerConfigKnowledgeModelEditor where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "KNOWLEDGE_MODEL_EDITOR_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigKnowledgeModelEditor)
      , \c -> applyStringEnvVariable "KNOWLEDGE_MODEL_EDITOR_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigKnowledgeModelEditor)
      ]

instance FromEnv ServerConfigProject where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "PROJECT_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigProject)
      , \c -> applyEnvVariable "PROJECT_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigProject)
      , \c -> applyEnvVariable "PROJECT_ASSIGNEE_NOTIFICATION_ENABLED" c.assigneeNotification.enabled (\x -> c {assigneeNotification = c.assigneeNotification {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_ASSIGNEE_NOTIFICATION_CRON" c.assigneeNotification.cron (\x -> c {assigneeNotification = c.assigneeNotification {cron = x}} :: ServerConfigProject)
      ]

instance FromEnv ServerConfigTemporaryFile where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "TEMPORARY_FILE_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigTemporaryFile)
      , \c -> applyStringEnvVariable "TEMPORARY_FILE_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigTemporaryFile)
      ]

instance FromEnv ServerConfigWebsocket where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "WEBSOCKET_CLEAN_CONNECTIONS_ENABLED" c.cleanConnections.enabled (\x -> c {cleanConnections = c.cleanConnections {enabled = x}} :: ServerConfigWebsocket)
      , \c -> applyStringEnvVariable "WEBSOCKET_CLEAN_CONNECTIONS_CRON" c.cleanConnections.cron (\x -> c {cleanConnections = c.cleanConnections {cron = x}} :: ServerConfigWebsocket)
      , \c -> applyEnvVariable "WEBSOCKET_CLEAN_OUTBOX_ENABLED" c.cleanOutbox.enabled (\x -> c {cleanOutbox = c.cleanOutbox {enabled = x}} :: ServerConfigWebsocket)
      , \c -> applyStringEnvVariable "WEBSOCKET_CLEAN_OUTBOX_CRON" c.cleanOutbox.cron (\x -> c {cleanOutbox = c.cleanOutbox {cron = x}} :: ServerConfigWebsocket)
      ]

instance FromEnv ServerConfigUserToken where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "USER_TOKEN_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigUserToken)
      , \c -> applyStringEnvVariable "USER_TOKEN_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigUserToken)
      , \c -> applyEnvVariable "USER_TOKEN_EXPIRE_ENABLED" c.expire.enabled (\x -> c {expire = c.expire {enabled = x}} :: ServerConfigUserToken)
      , \c -> applyStringEnvVariable "USER_TOKEN_EXPIRE_CRON" c.expire.cron (\x -> c {expire = c.expire {cron = x}} :: ServerConfigUserToken)
      ]

instance FromEnv ServerConfigAdmin where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ADMIN_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigAdmin)
      ]

instance FromEnv ServerConfigRegistry where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "REGISTRY_URL" c.url (\x -> c {url = x} :: ServerConfigRegistry)
      , \c -> applyStringEnvVariable "REGISTRY_CLIENT_URL" c.clientUrl (\x -> c {clientUrl = x} :: ServerConfigRegistry)
      , \c -> applyEnvVariable "REGISTRY_SYNC_ENABLED" c.sync.enabled (\x -> c {sync = c.sync {enabled = x}} :: ServerConfigRegistry)
      , \c -> applyStringEnvVariable "REGISTRY_SYNC_CRON" c.sync.cron (\x -> c {sync = c.sync {cron = x}} :: ServerConfigRegistry)
      ]
