module Shared.Service.OpenId.Client.Flow.OpenIdClientFlowService where

import qualified Control.Exception.Base as E
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.IdTokenFlow as O_ID
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Localization.Messages.OpenId.Public
import Shared.Model.Context.RequestContext
import Shared.Model.Error.Error
import Shared.Model.OpenId.OpenIdClientParameter
import Shared.Util.Crypto (generateRandomString)
import Shared.Util.Logger

createAuthenticationUrl' :: RequestContextC s sc m => O.OIDC -> [OpenIdClientParameter] -> Maybe String -> Maybe String -> m ()
createAuthenticationUrl' openIDClient parameters mFlow mClientUrl = do
  state <- liftIO $ generateRandomString 40
  let nonce = "FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL"
  let params = fmap (\p -> (BS.pack p.name, Just . BS.pack $ p.value)) parameters ++ [("nonce", Just . BS.pack $ nonce)]
  loc <-
    case mFlow of
      Just "id_token" -> liftIO $ O_ID.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
      _ -> liftIO $ O.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
  throwError $ FoundError (show loc)

requestTokensWithCode :: RequestContextC s sc m => O.OIDC -> Maybe String -> Maybe String -> m (OT.Tokens A.Value)
requestTokensWithCode openIDClient mCode mNonce =
  case mCode of
    Just code -> do
      context <- ask
      eTokens <- liftIO . E.try $ O.requestTokens openIDClient (fmap BS.pack mNonce) (BS.pack code) context.httpClientManager' :: RequestContextC s sc m => m (Either E.SomeException (OT.Tokens A.Value))
      case eTokens of
        Right tokens -> return tokens
        Left error -> do
          logWarnI _CMP_SERVICE . f' "Error in retrieving token from OpenID (error: '%s')" $ [show error]
          throwError . UserError . _ERROR_VALIDATION__OPENID_WRONG_RESPONSE . show $ error
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__OPENID_CODE_ABSENCE
