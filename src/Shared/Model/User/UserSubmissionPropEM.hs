module Shared.Model.User.UserSubmissionPropEM where

import qualified Data.Map.Strict as M

import Shared.Model.Common.SensitiveData
import Shared.Model.User.UserSubmissionProp
import Shared.Model.User.UserSubmissionPropList
import Shared.Util.Crypto (encryptAES256WithB64)

instance SensitiveData UserSubmissionProp where
  process key entity =
    entity {values = M.map (encryptAES256WithB64 key) entity.values}

instance SensitiveData UserSubmissionPropList where
  process key entity =
    entity {values = M.map (encryptAES256WithB64 key) entity.values}
