module Shared.Database.Mapping.ExternalLink.ExternalLinkUsage where

import Database.PostgreSQL.Simple

import Shared.Model.ExternalLink.ExternalLinkUsage

instance ToRow ExternalLinkUsage

instance FromRow ExternalLinkUsage
