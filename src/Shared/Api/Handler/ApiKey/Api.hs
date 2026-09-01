module Shared.Api.Handler.ApiKey.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.ApiKey.Detail_DELETE
import Shared.Api.Handler.ApiKey.List_GET
import Shared.Api.Handler.ApiKey.List_POST
import Shared.Api.Handler.WizardCommon

type ApiKeyAPI =
  Tags "ApiKey"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_DELETE
       )

apiKeyApi :: Proxy ApiKeyAPI
apiKeyApi = Proxy

apiKeyServer :: WizardHandlerC s sm r rm => ServerT ApiKeyAPI sm
apiKeyServer = list_GET :<|> list_POST :<|> detail_DELETE
