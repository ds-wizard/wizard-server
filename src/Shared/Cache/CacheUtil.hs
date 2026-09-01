module Shared.Cache.CacheUtil where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Cache as C
import GHC.Int

import Shared.Api.Resource.UserToken.UserTokenJM ()
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.WizardRequestContext

purgeCache :: WizardRequestContextC s m => m ()
purgeCache = do
  cache <- asks (.cache')
  liftIO . C.purge $ cache.knowledgeModelEditorWebsocket
  liftIO . C.purge $ cache.projectWebsocket
  liftIO . C.purge $ cache.userToken

purgeExpiredCache :: WizardRequestContextC s m => m ()
purgeExpiredCache = do
  cache <- asks (.cache')
  liftIO . C.purgeExpired $ cache.knowledgeModelEditorWebsocket
  liftIO . C.purgeExpired $ cache.projectWebsocket
  liftIO . C.purgeExpired $ cache.userToken

computeUserTokenCacheSize :: WizardRequestContextC s m => m String
computeUserTokenCacheSize = computeCacheSize (.userToken)

computeCacheSize :: (WizardRequestContextC s m, ToJSON value) => (ServerCache -> C.Cache key value) -> m String
computeCacheSize cacheAccessor = do
  cache <- asks (.cache')
  items <- liftIO . C.toList . cacheAccessor $ cache
  let values = fmap (\(k, v, exp) -> v) items
  return . formatByteSize . BSL.length . encode $ values

formatByteSize :: Int64 -> String
formatByteSize size
  | size < 1024 = show size ++ " B"
  | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
  | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
  | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"
