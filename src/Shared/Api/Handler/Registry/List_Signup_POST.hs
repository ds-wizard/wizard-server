module Shared.Api.Handler.Registry.List_Signup_POST where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Registry.RegistryCreateDTO
import Shared.Api.Resource.Registry.RegistryCreateJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Registry.Registration.RegistryRegistrationService

type List_Signup_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] RegistryCreateDTO
    :> "registry"
    :> "signup"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_signup_POST
  :: WizardHandlerC s sm r rm => Maybe String -> RegistryCreateDTO -> sm (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_signup_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< signUpToRegistry reqDto
