{-# LANGUAGE KindSignatures #-}

module Shared.Api.Handler.Settings.List_DELETE where

import qualified Data.UUID as U
import GHC.TypeLits (Symbol)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState

type List_DELETE (section :: Symbol) =
  Header "Authorization" String
    :> Header "Host" String
    :> "settings"
    :> section
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE :: WizardHandlerC s sm r rm => rm () -> Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE deleteSettings mTokenHeader mServerUrl mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteSettings
        return NoContent
