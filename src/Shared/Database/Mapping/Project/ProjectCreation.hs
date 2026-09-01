module Shared.Database.Mapping.Project.ProjectCreation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Tenant.Config.WizardTenantConfig

instance ToField ProjectCreation where
  toField = toFieldGenericEnum

instance FromField ProjectCreation where
  fromField = fromFieldGenericEnum
