module Shared.Model.Config.PublicServerConfigIM where

import Shared.Model.Config.ServerConfigIM

import Shared.Model.Config.PublicServerConfig

instance FromEnv ServerConfigExternalLink where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "EXTERNAL_LINK_ALLOWED_DOMAINS" c.allowedDomains (\x -> c {allowedDomains = x})
      ]
