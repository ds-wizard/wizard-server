module Shared.Database.Mapping.OpenId.OpenIdClientSession where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.OpenId.OpenIdClientSession

instance FromRow OpenIdClientSession

instance ToRow OpenIdClientSession
