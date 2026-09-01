module Shared.Model.Config.WizardServerConfig where

import qualified Crypto.PubKey.RSA as RSA
import GHC.Generics
import GHC.Records

import Shared.Model.Config.PublicServerConfig
import Shared.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , aws :: ServerConfigAws
  , sentry :: ServerConfigSentry
  , userEmailLink :: ServerConfigUserEmailLink
  , cache :: ServerConfigCache
  , document :: ServerConfigDocument
  , externalLink :: ServerConfigExternalLink
  , knowledgeModelEditor :: ServerConfigKnowledgeModelEditor
  , project :: ServerConfigProject
  , temporaryFile :: ServerConfigTemporaryFile
  , userToken :: ServerConfigUserToken
  , userRegistration :: ServerConfigUserRegistration
  , analyticalMails :: ServerConfigAnalyticalMails
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  , persistentCommand :: ServerConfigPersistentCommand
  , signalBridge :: ServerConfigSignalBridge
  , admin :: ServerConfigAdmin
  , registry :: ServerConfigRegistry
  , httpClient :: ServerConfigHttpClient
  }
  deriving (Generic, Show)

data ServerConfigHttpClient = ServerConfigHttpClient
  { restricted :: ServerConfigHttpClientRestricted
  }
  deriving (Generic, Show)

data ServerConfigHttpClientRestricted = ServerConfigHttpClientRestricted
  { allowedHosts :: [String]
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: String
  , clientUrl :: String
  , serverPort :: Int
  , secret :: String
  , rsaPrivateKey :: RSA.PrivateKey
  , integrationConfig :: String
  }
  deriving (Generic, Show)

data ServerConfigUserEmailLink = ServerConfigUserEmailLink
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigCache = ServerConfigCache
  { dataExpiration :: Integer
  , websocketExpiration :: Integer
  , purgeExpired :: ServerConfigCronWorker
  , dataEnabled :: Bool
  }
  deriving (Generic, Show)

data ServerConfigDocument = ServerConfigDocument
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigKnowledgeModelEditor = ServerConfigKnowledgeModelEditor
  { squash :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigProject = ServerConfigProject
  { clean :: ServerConfigCronWorker
  , squash :: ServerConfigCronWorker
  , assigneeNotification :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigTemporaryFile = ServerConfigTemporaryFile
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigUserToken = ServerConfigUserToken
  { clean :: ServerConfigCronWorker
  , expire :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigUserRegistration = ServerConfigUserRegistration
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigSignalBridge = ServerConfigSignalBridge
  { enabled :: Bool
  , updatePermsArn :: String
  , updateUserGroupArn :: String
  , setProjectArn :: String
  , addEventArn :: String
  , addFileArn :: String
  , logOutAllArn :: String
  }
  deriving (Generic, Show)

data ServerConfigAdmin = ServerConfigAdmin
  { enabled :: Bool
  , serverUrl :: String
  }
  deriving (Generic, Show)

data ServerConfigRegistry = ServerConfigRegistry
  { url :: String
  , clientUrl :: String
  , sync :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

instance HasField "serverPort'" ServerConfig Int where
  getField = (.general.serverPort)

instance HasField "environment'" ServerConfig String where
  getField = (.general.environment)

instance HasField "database'" ServerConfig ServerConfigDatabase where
  getField = (.database)

instance HasField "s3'" ServerConfig ServerConfigS3 where
  getField = (.s3)

instance HasField "sentry'" ServerConfig ServerConfigSentry where
  getField = (.sentry)

instance HasField "logging'" ServerConfig ServerConfigLogging where
  getField = (.logging)

instance HasField "cloud'" ServerConfig ServerConfigCloud where
  getField = (.cloud)

instance HasField "persistentCommand'" ServerConfig ServerConfigPersistentCommand where
  getField = (.persistentCommand)

instance HasField "aws'" ServerConfig ServerConfigAws where
  getField = (.aws)

instance HasField "cache'" ServerConfig ServerConfigCache where
  getField = (.cache)

instance HasField "externalLink'" ServerConfig ServerConfigExternalLink where
  getField = (.externalLink)

instance HasField "dataEnabled'" ServerConfigCache Bool where
  getField = (.dataEnabled)
