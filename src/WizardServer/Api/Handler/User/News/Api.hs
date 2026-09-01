module WizardServer.Api.Handler.User.News.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.User.News.Detail_PUT

type NewsAPI =
  Tags "User News"
    :> Detail_PUT

newsApi :: Proxy NewsAPI
newsApi = Proxy

newsServer :: WizardHandlerC s sm r rm => ServerT NewsAPI sm
newsServer = detail_PUT
