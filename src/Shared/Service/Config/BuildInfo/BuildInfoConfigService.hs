module Shared.Service.Config.BuildInfo.BuildInfoConfigService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.BuildInfoConfigJM ()
import Shared.Model.Error.Error

getBuildInfoConfig :: String -> IO (Either AppError BuildInfoConfig)
getBuildInfoConfig fileNameBase = do
  mFileNameEnv <- liftIO $ lookupEnv "BUILD_INFO_PATH"
  let fileName = fromMaybe fileNameBase mFileNameEnv
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
