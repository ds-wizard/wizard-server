module Shared.Cache.CacheFactory where

import qualified Data.Cache as C

import Shared.Model.Cache.ServerCache
import Shared.Util.Clock

createServerCache serverConfig = do
  let [dataExp, websocketExp] = fmap (Just . fromHoursToTimeSpec) [serverConfig.cache.dataExpiration, serverConfig.cache.websocketExpiration]
  knowledgeModelEditorWebsocket <- C.newCache websocketExp
  projectWebsocket <- C.newCache websocketExp
  userToken <- C.newCache dataExp
  return ServerCache {..}
