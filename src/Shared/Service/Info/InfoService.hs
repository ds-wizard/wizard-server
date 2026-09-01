module Shared.Service.Info.InfoService where

import Control.Monad.Reader (asks)

import Shared.Api.Resource.Info.InfoDTO
import Shared.Database.DAO.Component.ComponentDAO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Context.RequestContext

getInfo :: RequestContextC s sc m => [InfoMetamodelVersionDTO] -> m InfoDTO
getInfo metamodelVersions = do
  buildInfoConfig <- asks (.buildInfoConfig')
  components <- findComponents
  return
    InfoDTO
      { name = buildInfoConfig.name
      , version = buildInfoConfig.version
      , builtAt = buildInfoConfig.builtAt
      , components = components
      , metamodelVersions = metamodelVersions
      }
