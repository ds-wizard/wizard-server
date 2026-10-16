module Shared.Cache.CacheFactory where

import qualified Data.Cache as C

import Shared.Model.Cache.ServerCache
import Shared.Util.Clock

createServerCache serverConfig = do
  userToken <- C.newCache (Just . fromHoursToTimeSpec $ serverConfig.cache.dataExpiration)
  return ServerCache {..}
