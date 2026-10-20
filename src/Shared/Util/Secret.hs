module Shared.Util.Secret where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Error.Error
import Shared.Util.String (replace)

maskedSecret :: String
maskedSecret = "********"

maskSecret :: String -> String
maskSecret "" = ""
maskSecret _ = maskedSecret

maskSecrets :: M.Map k String -> M.Map k String
maskSecrets = M.map maskSecret

unmaskSecret :: String -> String -> String
unmaskSecret current value
  | value == maskedSecret = current
  | otherwise = value

unmaskSecrets :: Ord k => M.Map k String -> M.Map k String -> M.Map k String
unmaskSecrets current = M.mapWithKey (\key -> unmaskSecret (M.findWithDefault "" key current))

unmaskFor :: Bool -> String -> String -> String -> Either AppError String
unmaskFor targetChanged label current value
  | targetChanged && value == maskedSecret = Left . UserError $ _ERROR_SERVICE_MCP__MASKED_SECRET_TARGET_CHANGED label
  | otherwise = Right (unmaskSecret current value)

unmaskSecretsFor :: Ord k => Bool -> String -> M.Map k String -> M.Map k String -> Either AppError (M.Map k String)
unmaskSecretsFor targetChanged label current = M.traverseWithKey (\key -> unmaskFor targetChanged label (M.findWithDefault "" key current))

secretPlaceholder :: String -> String
secretPlaceholder name = "[SECRET:" ++ name ++ "]"

redactSecretNames :: M.Map String String -> M.Map String String
redactSecretNames = M.mapWithKey (\name _ -> secretPlaceholder name)

redactSecretValues :: M.Map String String -> String -> String
redactSecretValues secrets text = foldr redact text (L.sortOn (length . snd) . filter (not . null . snd) . M.toList $ secrets)
  where
    redact (name, value) = replace value (secretPlaceholder name)
