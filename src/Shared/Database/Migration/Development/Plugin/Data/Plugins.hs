module Shared.Database.Migration.Development.Plugin.Data.Plugins where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Constant.Tenant
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.PluginList
import Shared.Util.Date
import Shared.Util.Uuid

pluginDict :: M.Map U.UUID Bool
pluginDict = M.fromList [(plugin1.uuid, False)]

plugin1 :: Plugin
plugin1 =
  Plugin
    { uuid = u' "13f297a0-d8b3-4efe-b65a-c78117dabad8"
    , url = "https://example.com/plugin1"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

plugin1List :: PluginList
plugin1List =
  PluginList
    { uuid = plugin1.uuid
    , url = plugin1.url
    , enabled = plugin1.enabled
    }

differentPlugin1 :: Plugin
differentPlugin1 =
  Plugin
    { uuid = u' "13f297a0-d8b3-4efe-b65a-c78117dabad8"
    , url = "https://example.com/plugin1"
    , enabled = True
    , tenantUuid = differentTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
