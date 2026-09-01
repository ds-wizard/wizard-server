module RegistryPublic.Model.Organization.OrganizationRole where

import GHC.Generics

data OrganizationRole
  = AdminRole
  | UserRole
  deriving (Show, Eq, Generic, Read)
