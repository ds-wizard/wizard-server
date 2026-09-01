module Shared.Api.Handler.Prefab.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Prefab.PrefabJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Prefab.Prefab
import Shared.Service.Prefab.PrefabService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "prefabs"
    :> QueryParam "type" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [Prefab])

list_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [Prefab])
list_GET mTokenHeader mServerUrl prefabType =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "type" <$> prefabType]
        getPrefabsFiltered queryParams
