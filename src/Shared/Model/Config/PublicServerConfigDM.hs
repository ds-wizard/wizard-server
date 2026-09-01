module Shared.Model.Config.PublicServerConfigDM where

import Shared.Model.Config.PublicServerConfig

defaultExternalLink :: ServerConfigExternalLink
defaultExternalLink =
  ServerConfigExternalLink
    { allowedDomains = ["ds-wizard.org"]
    }
