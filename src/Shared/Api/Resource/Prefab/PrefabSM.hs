module Shared.Api.Resource.Prefab.PrefabSM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.Prefab.PrefabJM ()
import Shared.Database.Migration.Development.Prefab.Data.Prefabs
import Shared.Model.Prefab.Prefab
import Shared.Util.Swagger

instance ToSchema Prefab where
  declareNamedSchema = toSwagger kmIntegrationBioPortalPrefab
