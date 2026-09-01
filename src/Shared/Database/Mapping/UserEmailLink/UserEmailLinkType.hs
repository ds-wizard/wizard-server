module Shared.Database.Mapping.UserEmailLink.UserEmailLinkType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.UserEmailLink.UserEmailLinkType

instance ToField UserEmailLinkType where
  toField = toFieldGenericEnum

instance FromField UserEmailLinkType where
  fromField = fromFieldGenericEnum
