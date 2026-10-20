module Shared.Api.Handler.Mcp.Common where

import Control.Monad.Except (catchError)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Network.HTTP.Types as HTTP
import Servant

import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Error.McpOAuthErrorJM ()
import Shared.Constant.Api
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardServerContext
import Shared.Model.Mcp.McpOAuthError
import Shared.Service.Mcp.McpFlowMapper
import Shared.Service.Tenant.TenantHelper
import Shared.Util.String (f')

respondOAuth :: WizardServerContextC s sm => sm (Either McpOAuthError a) -> sm a
respondOAuth = respondOAuthWith []

respondOAuthWith :: WizardServerContextC s sm => [HTTP.Header] -> sm (Either McpOAuthError a) -> sm a
respondOAuthWith challengeHeaders action = either (throwError . toOAuthServerError challengeHeaders) return =<< action

toOAuthServerError :: [HTTP.Header] -> McpOAuthError -> ServerError
toOAuthServerError challengeHeaders oauthError =
  (byStatus oauthError.status) {errBody = encode oauthError, errHeaders = contentTypeHeaderJSON : [h | oauthError.status == 401, h <- challengeHeaders]}
  where
    byStatus 401 = err401
    byStatus 404 = err404
    byStatus _ = err400

basicChallenge :: Maybe String -> [HTTP.Header]
basicChallenge mAuthHeader
  | maybe False (("basic " `isPrefixOf`) . map toLower) mAuthHeader = [("WWW-Authenticate", "Basic realm=\"mcp\"")]
  | otherwise = []

withResourceMetadata :: WizardHandlerC s sm r rm => Maybe String -> sm a -> sm a
withResourceMetadata mServerUrl action = catchError action handleError
  where
    handleError error
      | errHTTPCode error == 401 = do
          issuer <- runInUnauthService mServerUrl NoTransaction (mcpIssuer <$> getCurrentTenant)
          let header = ("WWW-Authenticate", BS.pack $ f' "Bearer resource_metadata=\"%s/.well-known/oauth-protected-resource\"" [issuer])
          throwError error {errHeaders = header : errHeaders error}
      | otherwise = throwError error

mcpAccepted :: ServerError
mcpAccepted = ServerError {errHTTPCode = 202, errReasonPhrase = "Accepted", errBody = "", errHeaders = []}
