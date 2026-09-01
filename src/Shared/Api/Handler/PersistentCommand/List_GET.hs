module Shared.Api.Handler.PersistentCommand.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.PersistentCommand.PersistentCommandListJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Util.String (splitOn)

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> QueryParam "state" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandList))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandList))
list_GET mTokenHeader mServerUrl mStatesL mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let states =
              case mStatesL of
                Just statesL -> splitOn "," statesL
                Nothing -> []
        getPersistentCommandsPage states (Pageable mPage mSize) (parseSortQuery mSort)
