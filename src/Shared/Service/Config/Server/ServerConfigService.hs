module Shared.Service.Config.Server.ServerConfigService where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither')

import Shared.Model.Config.ServerConfigIM
import Shared.Model.Error.Error

getServerConfig :: (FromJSON serverConfig, FromEnv serverConfig) => (serverConfig -> Either AppError serverConfig) -> ByteString -> IO (Either AppError serverConfig)
getServerConfig validateServerConfig bs =
  case decodeEither' bs of
    Right value -> getServerConfigFromValue validateServerConfig value
    Left error -> return . Left . GeneralServerError . show $ error

getServerConfigFromValue :: (FromJSON serverConfig, FromEnv serverConfig) => (serverConfig -> Either AppError serverConfig) -> Value -> IO (Either AppError serverConfig)
getServerConfigFromValue validateServerConfig value =
  case fromJSON value of
    Success config -> do
      updatedConfig <- applyEnv config
      return . validateServerConfig $ updatedConfig
    Error error -> return . Left . GeneralServerError $ error
