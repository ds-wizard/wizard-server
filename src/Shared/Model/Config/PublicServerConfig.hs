module Shared.Model.Config.PublicServerConfig where

import GHC.Generics

data ServerConfigExternalLink = ServerConfigExternalLink
  { allowedDomains :: [String]
  }
  deriving (Generic, Show)
