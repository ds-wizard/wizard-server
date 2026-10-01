module Shared.Api.Handler.DocumentTemplate.Dependent.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.DocumentTemplate.Dependent.List_GET
import Shared.Api.Handler.WizardCommon

type DependentAPI =
  Tags "Document Template Dependent"
    :> List_GET

dependentApi :: Proxy DependentAPI
dependentApi = Proxy

dependentServer :: WizardHandlerC s sm r rm => ServerT DependentAPI sm
dependentServer =
  list_GET
