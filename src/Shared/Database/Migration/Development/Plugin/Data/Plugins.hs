module Shared.Database.Migration.Development.Plugin.Data.Plugins where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Constant.Tenant
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.PluginList
import Shared.Util.Date
import Shared.Util.Uuid

pluginDict :: M.Map U.UUID PluginSettingsChangeDTO
pluginDict = M.fromList [(plugin1.uuid, PluginSettingsChangeDTO {enabled = False, workspaceOverrideAllowed = Nothing})]

plugin1 :: Plugin
plugin1 =
  Plugin
    { uuid = u' "13f297a0-d8b3-4efe-b65a-c78117dabad8"
    , url = "https://example.com/plugin1"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    , workspaceOverrideAllowed = True
    }

plugin1List :: PluginList
plugin1List =
  PluginList
    { uuid = plugin1.uuid
    , url = plugin1.url
    , enabled = plugin1.enabled
    }

plugin2 :: Plugin
plugin2 =
  Plugin
    { uuid = u' "6c2f0b6e-4a8d-4f1e-9b3c-2d7e5a1f8c04"
    , url = "https://example.com/plugin2"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    , workspaceOverrideAllowed = True
    }

plugin2List :: PluginList
plugin2List =
  PluginList
    { uuid = plugin2.uuid
    , url = plugin2.url
    , enabled = plugin2.enabled
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
    , workspaceOverrideAllowed = True
    }
