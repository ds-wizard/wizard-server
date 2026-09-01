module Shared.Api.Resource.Tenant.TenantDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Tenant.Tenant

data TenantDetailDTO = TenantDetailDTO
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , state :: TenantState
  , enabled :: Bool
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , usage :: WizardUsageDTO
  , users :: [UserDTO]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic)
