module Shared.Model.Settings.SettingsEM where

import Shared.Model.Common.SensitiveData
import Shared.Model.Settings.Settings
import Shared.Util.Crypto (encryptAES256WithB64)

instance SensitiveData SettingsRegistry where
  process key entity = entity {apiKey = encryptAES256WithB64 key entity.apiKey}
