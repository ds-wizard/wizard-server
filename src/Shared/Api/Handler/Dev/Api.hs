module Shared.Api.Handler.Dev.Api where

import Servant

import Shared.Api.Handler.Dev.Operation.Api
import Shared.Api.Handler.WizardCommon

type DevAPI = DevOperationAPI

devApi :: Proxy DevAPI
devApi = Proxy

devServer :: WizardHandlerC s sm r rm => ServerT DevAPI sm
devServer = devOperationServer
