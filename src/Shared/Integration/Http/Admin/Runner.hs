module Shared.Integration.Http.Admin.Runner where

import Control.Monad.Reader (asks)
import qualified Jose.Jwk as JWK

import Shared.Integration.Http.Admin.RequestMapper
import Shared.Integration.Http.Admin.ResponseMapper
import Shared.Integration.Http.Common.HttpClient
import Shared.Model.Context.WizardRequestContext

retrieveJwtPublicKeys :: WizardRequestContextC s m => m JWK.JwkSet
retrieveJwtPublicKeys = do
  serverConfig <- asks (.serverConfig')
  runRequest (toRetrieveJwtPublicKeysRequest serverConfig) toRetrieveJwtPublicKeysResponse
