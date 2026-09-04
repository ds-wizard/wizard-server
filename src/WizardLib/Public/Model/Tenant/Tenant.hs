module WizardLib.Public.Model.Tenant.Tenant where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Tenant = Tenant
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , state :: TenantState
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data TenantState
  = NotSeededTenantState
  | PendingHousekeepingTenantState
  | HousekeepingInProgressTenantState
  | ReadyForUseTenantState
  deriving (Show, Eq, Generic, Read)
