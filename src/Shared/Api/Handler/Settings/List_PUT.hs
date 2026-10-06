{-# LANGUAGE KindSignatures #-}

module Shared.Api.Handler.Settings.List_PUT where

import qualified Data.UUID as U
import GHC.TypeLits (Symbol)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Model.Context.TransactionState

type List_PUT (section :: Symbol) a =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] (SettingsDTO a)
    :> "settings"
    :> section
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (SettingsDTO a))

list_PUT
  :: WizardHandlerC s sm r rm
  => (SettingsDTO a -> rm (SettingsDTO a))
  -> Maybe String
  -> Maybe String
  -> SettingsDTO a
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] (SettingsDTO a))
list_PUT modifySettings mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifySettings reqDto
