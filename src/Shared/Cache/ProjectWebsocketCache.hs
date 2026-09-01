module Shared.Cache.ProjectWebsocketCache where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.Cache.Common
import Shared.Util.String

cacheName = "Project Websocket"

cacheKey connectionUuid = f' "connection: '%s'" [U.toString connectionUuid]

addToCache :: WizardRequestContextC s m => WebsocketRecord -> m ()
addToCache record = do
  let key = cacheKey record.connectionUuid
  logCacheAddBefore cacheName key
  projectCache <- getCache
  liftIO $ C.insert projectCache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: WizardRequestContextC s m => m [WebsocketRecord]
getAllFromCache = do
  projectCache <- getCache
  records <- liftIO $ C.toList projectCache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: WizardRequestContextC s m => U.UUID -> m (Maybe WebsocketRecord)
getFromCache connectionUuid = do
  let key = cacheKey connectionUuid
  logCacheGetBefore cacheName key
  projectCache <- getCache
  mValue <- liftIO $ C.lookup projectCache (H.hash key)
  case mValue of
    Just value -> do
      logCacheGetFound cacheName key
      return . Just $ value
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing

updateCache :: WizardRequestContextC s m => WebsocketRecord -> m ()
updateCache record = do
  let key = cacheKey record.connectionUuid
  logCacheModifyBefore cacheName key
  projectCache <- getCache
  liftIO $ C.insert projectCache (H.hash key) record
  logCacheModifyAfter cacheName key
  return ()

getFromCache' :: WizardRequestContextC s m => U.UUID -> m WebsocketRecord
getFromCache' connectionUuid = do
  mRecord <- getFromCache connectionUuid
  case mRecord of
    Just record -> return record
    Nothing -> throwError . NotExistsError . _ERROR_API__WEBSOCKET_RECORD_NOT_FOUND $ U.toString connectionUuid

deleteFromCache :: WizardRequestContextC s m => U.UUID -> m ()
deleteFromCache connectionUuid = do
  let key = cacheKey connectionUuid
  logCacheDeleteBefore cacheName key
  projectCache <- getCache
  liftIO $ C.delete projectCache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: WizardRequestContextC s m => m Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: WizardRequestContextC s m => m (C.Cache Int WebsocketRecord)
getCache = do
  cache <- asks (.cache')
  return cache.projectWebsocket
