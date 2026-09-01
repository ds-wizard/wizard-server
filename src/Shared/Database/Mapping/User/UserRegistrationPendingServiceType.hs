module Shared.Database.Mapping.User.UserRegistrationPendingServiceType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.User.UserRegistrationPendingServiceType

instance ToField UserRegistrationPendingServiceType where
  toField = toFieldGenericEnum

instance FromField UserRegistrationPendingServiceType where
  fromField = fromFieldGenericEnum
