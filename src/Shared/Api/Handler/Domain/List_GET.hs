module Shared.Api.Handler.Domain.List_GET where

import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Tenant.TenantValidation

type List_GET =
  Header "Host" String
    :> "domains"
    :> QueryParam "check-domain" String
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_GET mServerUrl mTenantId =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< do
      validateTenantId (fromMaybe "" mTenantId)
      return NoContent
