module Shared.Service.Cache.Common where

import Shared.Model.Context.RequestContext
import Shared.Util.Logger

logCacheWarmBefore :: RequestContextC s sc m => String -> m ()
logCacheWarmBefore cacheName = logDebugI _CMP_CACHE (f' "Warm (before): %s" [cacheName])

logCacheWarmAfter :: RequestContextC s sc m => String -> m ()
logCacheWarmAfter cacheName = logDebugI _CMP_CACHE (f' "Warm (after): %s" [cacheName])

logCacheAddBefore :: RequestContextC s sc m => String -> String -> m ()
logCacheAddBefore cacheName key = logDebugI _CMP_CACHE (f' "Add (before): %s, key: { %s }" [cacheName, key])

logCacheAddAfter :: RequestContextC s sc m => String -> String -> m ()
logCacheAddAfter cacheName key = logDebugI _CMP_CACHE (f' "Add (after): %s, key: { %s }" [cacheName, key])

logCacheGetBefore :: RequestContextC s sc m => String -> String -> m ()
logCacheGetBefore cacheName key = logDebugI _CMP_CACHE (f' "Get (before): '%s', key: { %s }" [cacheName, key])

logCacheGetFound :: RequestContextC s sc m => String -> String -> m ()
logCacheGetFound cacheName key = logDebugI _CMP_CACHE (f' "Get (found): '%s', key: { %s }" [cacheName, key])

logCacheGetMissed :: RequestContextC s sc m => String -> String -> m ()
logCacheGetMissed cacheName key = logDebugI _CMP_CACHE (f' "Get (missed): '%s', key: { %s }" [cacheName, key])

logCacheModifyBefore :: RequestContextC s sc m => String -> String -> m ()
logCacheModifyBefore cacheName key = logDebugI _CMP_CACHE (f' "Modify (before): %s, key: { %s }" [cacheName, key])

logCacheModifyAfter :: RequestContextC s sc m => String -> String -> m ()
logCacheModifyAfter cacheName key = logDebugI _CMP_CACHE (f' "Modify (after): %s, key: { %s }" [cacheName, key])

logCacheDeleteAllBefore :: RequestContextC s sc m => String -> m ()
logCacheDeleteAllBefore cacheName = logDebugI _CMP_CACHE (f' "Delete All (before): '%s'" [cacheName])

logCacheDeleteAllFinished :: RequestContextC s sc m => String -> m ()
logCacheDeleteAllFinished cacheName = logDebugI _CMP_CACHE (f' "Delete All (finished): '%s'" [cacheName])

logCacheDeleteBefore :: RequestContextC s sc m => String -> String -> m ()
logCacheDeleteBefore cacheName key = logDebugI _CMP_CACHE (f' "Delete (before): '%s', key: { %s }" [cacheName, key])

logCacheDeleteFinished :: RequestContextC s sc m => String -> String -> m ()
logCacheDeleteFinished cacheName key = logDebugI _CMP_CACHE (f' "Delete (finished): '%s', key: { %s }" [cacheName, key])
