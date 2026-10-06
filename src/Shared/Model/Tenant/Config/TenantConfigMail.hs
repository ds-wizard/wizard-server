module Shared.Model.Tenant.Config.TenantConfigMail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantConfigMail = TenantConfigMail
  { tenantUuid :: U.UUID
  , configUuid :: Maybe U.UUID
  , customTemplates :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)
