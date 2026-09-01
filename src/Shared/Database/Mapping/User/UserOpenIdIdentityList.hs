module Shared.Database.Mapping.User.UserOpenIdIdentityList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Model.User.UserOpenIdIdentityList

instance FromRow UserOpenIdIdentityList where
  fromRow = do
    uuid <- field
    externalId <- field
    externalLabel <- field
    providerUuid <- field
    providerName <- field
    providerStyle <- fieldWith fromJSONField
    createdAt <- field
    return $ UserOpenIdIdentityList {..}
