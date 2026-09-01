module Shared.Service.Tenant.Limit.LimitService where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Localization.Messages.Public
import Shared.Model.Context.RequestContext
import Shared.Model.Error.Error

checkLimit :: (RequestContextC s sc m, Show number, Ord number, Num number) => String -> number -> number -> m ()
checkLimit name count maxCount =
  when (count >= abs maxCount) (throwError . UserError $ _ERROR_SERVICE_TENANT__LIMIT_EXCEEDED name count maxCount)
