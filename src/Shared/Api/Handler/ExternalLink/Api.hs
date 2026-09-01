module Shared.Api.Handler.ExternalLink.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.ExternalLink.List_GET
import Shared.Api.Handler.WizardCommon

type ExternalLinkAPI =
  Tags "ExternalLink"
    :> List_GET

externalLinkApi :: Proxy ExternalLinkAPI
externalLinkApi = Proxy

externalLinkServer :: WizardHandlerC s sm r rm => ServerT ExternalLinkAPI sm
externalLinkServer = list_GET
