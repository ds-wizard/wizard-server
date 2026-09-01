module Shared.Api.Handler.Registry.List_Confirmation_POST where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Registry.RegistryConfirmationDTO
import Shared.Api.Resource.Registry.RegistryConfirmationJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Registry.Registration.RegistryRegistrationService

type List_Confirmation_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] RegistryConfirmationDTO
    :> "registry"
    :> "confirmation"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_confirmation_POST
  :: WizardHandlerC s sm r rm => Maybe String -> RegistryConfirmationDTO -> sm (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_confirmation_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< confirmRegistration reqDto
