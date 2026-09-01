module Shared.Database.Mapping.Project.ProjectVisibility where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Project.Project

instance ToField ProjectVisibility where
  toField = toFieldGenericEnum

instance FromField ProjectVisibility where
  fromField = fromFieldGenericEnum
