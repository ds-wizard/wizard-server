module Shared.Service.Mcp.Tool.McpConfirmation where

import Control.Monad (when, (<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Data.UUID as U
import Text.Read (readMaybe)

import Shared.Constant.Mcp
import Shared.Database.DAO.Mcp.McpUsedConfirmationTokenDAO
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.Crypto (hashSHA256, hmacSHA256)
import Shared.Util.JSON (encodeJsonToString)
import Shared.Util.JsonSchema
import Shared.Util.String (splitOn)
import Shared.Util.Uuid (generateUuid)

data McpConfirmation m = McpConfirmation
  { summary :: String
  , revision :: String
  , perform :: m Value
  }

confirmationAnnotations :: McpToolAnnotations
confirmationAnnotations = McpToolAnnotations {readOnlyHint = False, destructiveHint = True, idempotentHint = False, openWorldHint = False}

confirmationDescription :: String
confirmationDescription =
  "The first call returns a summary and a confirmation_token; show the summary to the user and repeat the same call with the token once they agree"

confirmationArguments :: Arguments a -> Arguments (Maybe String, a)
confirmationArguments arguments =
  (,)
    <$> optionalArgument "confirmation_token" "Leave it out to get a summary of the effect and a token; repeat the same call with the token to perform it"
    <*> arguments

confirmationOutputSchema :: Value -> Value
confirmationOutputSchema resultSchema = object ["type" .= ("object" :: String), "anyOf" .= [confirmationRequestSchema, resultSchema]]

confirmationRequestSchema :: Value
confirmationRequestSchema =
  objectSchema
    ["summary", "confirmation_token", "expires_at"]
    [ ("summary", describedSchema "What the call will do; show it to the user before you repeat the call with the token" stringSchema)
    , ("confirmation_token", stringSchema)
    , ("expires_at", formatSchema "date-time")
    ]

withConfirmation :: WizardRequestContextC s m => String -> Arguments a -> (a -> m (McpConfirmation m)) -> Value -> Either String (m Value)
withConfirmation toolName arguments prepare = withConfirmationWhen toolName arguments (fmap Right . prepare)

withConfirmationWhen :: WizardRequestContextC s m => String -> Arguments a -> (a -> m (Either (m Value) (McpConfirmation m))) -> Value -> Either String (m Value)
withConfirmationWhen toolName arguments prepare value =
  withArguments (confirmationArguments arguments) (\(mToken, parsed) -> either id (confirm mToken <=< bindings) =<< prepare parsed) value
  where
    bindings confirmation = (,) confirmation <$> confirmationBindings toolName value confirmation.revision

confirm :: WizardRequestContextC s m => Maybe String -> (McpConfirmation m, String) -> m Value
confirm Nothing (confirmation, bindingsHash) = do
  expiry <- (+ ceiling mcpConfirmationExpiration) . floor <$> liftIO getPOSIXTime
  nonce <- U.toString <$> liftIO generateUuid
  token <- signConfirmation expiry nonce bindingsHash
  return $ object ["summary" .= confirmation.summary, "confirmation_token" .= token, "expires_at" .= posixSecondsToUTCTime (fromInteger expiry)]
confirm (Just token) (confirmation, bindingsHash) = do
  expiry <- checkConfirmationToken bindingsHash token
  useConfirmationToken token expiry
  confirmation.perform

checkConfirmationToken :: WizardRequestContextC s m => String -> String -> m Integer
checkConfirmationToken bindingsHash token =
  case splitOn "." token of
    [expiryText, nonce, tokenBindingsHash, _]
      | Just expiry <- readMaybe expiryText
      , tokenBindingsHash == bindingsHash -> do
          expected <- signConfirmation expiry nonce tokenBindingsHash
          when (expected /= token) (throwError . UserError $ _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
          now <- liftIO getPOSIXTime
          when (fromInteger expiry < now) (throwError . UserError $ _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_EXPIRED)
          return expiry
    _ -> throwError . UserError $ _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH

useConfirmationToken :: WizardRequestContextC s m => String -> Integer -> m ()
useConfirmationToken token expiry = do
  inserted <- insertMcpUsedConfirmationToken (hashSHA256 token) (posixSecondsToUTCTime (fromInteger expiry))
  when (inserted == 0) (throwError . UserError $ _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_USED)

signConfirmation :: WizardRequestContextC s m => Integer -> String -> String -> m String
signConfirmation expiry nonce bindingsHash = do
  serverConfig <- asks (.serverConfig')
  return $ confirmationToken serverConfig.general.secret expiry nonce bindingsHash

confirmationToken :: String -> Integer -> String -> String -> String
confirmationToken secret expiry nonce bindingsHash =
  intercalate "." [show expiry, nonce, bindingsHash, hmacSHA256 secret (intercalate "." [show expiry, nonce, bindingsHash])]

confirmationBindings :: WizardRequestContextC s m => String -> Value -> String -> m String
confirmationBindings toolName arguments revision = do
  mUserUuid <- getCurrentUserUuid
  return . hashSHA256 $ intercalate "\n" [toolName, encodeArguments arguments, maybe "" U.toString mUserUuid, revision]

encodeArguments :: Value -> String
encodeArguments (Object o) = encodeJsonToString . Object $ KM.delete "confirmation_token" o
encodeArguments value = encodeJsonToString value
