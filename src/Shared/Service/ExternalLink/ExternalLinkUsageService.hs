module Shared.Service.ExternalLink.ExternalLinkUsageService where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Time
import GHC.Records
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme)
import Prelude hiding (id)

import Shared.Database.DAO.Common
import Shared.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import Shared.Localization.Messages.ExternalLink.Public
import Shared.Model.Config.PublicServerConfig
import Shared.Model.Context.RequestContext
import Shared.Model.Error.Error
import Shared.Model.ExternalLink.ExternalLinkUsage
import Shared.Util.Logger
import Shared.Util.Uuid

createExternalLinkUsage
  :: ( RequestContextC s sc m
     , HasField "externalLink'" sc ServerConfigExternalLink
     )
  => String
  -> m ()
createExternalLinkUsage url = do
  externalLinkCfg <- asks (.serverConfig'.externalLink')
  unless (isAllowedExternalLinkUrl externalLinkCfg.allowedDomains url) $
    throwError (ForbiddenError _ERROR_SERVICE_EXTERNAL_LINK__URL_NOT_ALLOWED)
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let externalLinkUsage = ExternalLinkUsage uuid url tenantUuid now
    void $ insertExternalLinkUsage externalLinkUsage

isAllowedExternalLinkUrl :: [String] -> String -> Bool
isAllowedExternalLinkUrl allowedDomains url =
  case parseURI url of
    Nothing -> False
    Just uri ->
      uriScheme uri `elem` ["http:", "https:"]
        && case uriAuthority uri of
          Nothing -> False
          Just auth ->
            let hostname = map toLower (uriRegName auth)
                domains = map (map toLower) allowedDomains
             in any (\d -> hostname == d || isSuffixOf ("." <> d) hostname) domains
