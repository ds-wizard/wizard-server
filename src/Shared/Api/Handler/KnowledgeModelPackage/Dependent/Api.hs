module Shared.Api.Handler.KnowledgeModelPackage.Dependent.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModelPackage.Dependent.List_GET
import Shared.Api.Handler.WizardCommon

type DependentAPI =
  Tags "Knowledge Model Package Dependent"
    :> List_GET

dependentApi :: Proxy DependentAPI
dependentApi = Proxy

dependentServer :: WizardHandlerC s sm r rm => ServerT DependentAPI sm
dependentServer =
  list_GET
