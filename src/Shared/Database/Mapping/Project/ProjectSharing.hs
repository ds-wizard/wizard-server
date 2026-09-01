module Shared.Database.Mapping.Project.ProjectSharing where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Project.Project

instance ToField ProjectSharing where
  toField = toFieldGenericEnum

instance FromField ProjectSharing where
  fromField = fromFieldGenericEnum
