{-# LANGUAGE KindSignatures #-}

module Shared.Api.Handler.Settings.List_GET where

import qualified Data.UUID as U
import GHC.TypeLits (Symbol)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Model.Context.TransactionState

type List_GET (section :: Symbol) a =
  Header "Authorization" String
    :> Header "Host" String
    :> "settings"
    :> section
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (SettingsDTO a))

list_GET :: WizardHandlerC s sm r rm => rm (SettingsDTO a) -> Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] (SettingsDTO a))
list_GET getSettings mTokenHeader mServerUrl mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getSettings
