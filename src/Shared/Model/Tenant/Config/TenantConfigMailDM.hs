module Shared.Model.Tenant.Config.TenantConfigMailDM where

import qualified Data.UUID as U

import Shared.Model.Tenant.Config.TenantConfigMail
import Shared.Util.Date

defaultMail :: TenantConfigMail
defaultMail =
  TenantConfigMail
    { tenantUuid = U.nil
    , configUuid = Nothing
    , customTemplates = False
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }
