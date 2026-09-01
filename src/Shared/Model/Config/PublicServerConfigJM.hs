module Shared.Model.Config.PublicServerConfigJM where

import Control.Monad (mzero)
import Data.Aeson

import Shared.Model.Config.PublicServerConfig
import Shared.Model.Config.PublicServerConfigDM

instance FromJSON ServerConfigExternalLink where
  parseJSON (Object o) = do
    allowedDomains <- o .:? "allowedDomains" .!= defaultExternalLink.allowedDomains
    return ServerConfigExternalLink {..}
  parseJSON _ = mzero
