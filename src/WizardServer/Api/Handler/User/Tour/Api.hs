module WizardServer.Api.Handler.User.Tour.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.User.Tour.Detail_PUT
import WizardServer.Api.Handler.User.Tour.List_DELETE

type TourAPI =
  Tags "User Tour"
    :> ( List_DELETE
           :<|> Detail_PUT
       )

tourApi :: Proxy TourAPI
tourApi = Proxy

tourServer :: WizardHandlerC s sm r rm => ServerT TourAPI sm
tourServer =
  list_DELETE
    :<|> detail_PUT
