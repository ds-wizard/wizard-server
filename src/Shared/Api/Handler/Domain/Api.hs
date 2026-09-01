module Shared.Api.Handler.Domain.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Domain.List_GET
import Shared.Api.Handler.WizardCommon

type DomainAPI =
  Tags "Domain"
    :> List_GET

domainApi :: Proxy DomainAPI
domainApi = Proxy

domainServer :: WizardHandlerC s sm r rm => ServerT DomainAPI sm
domainServer = list_GET
