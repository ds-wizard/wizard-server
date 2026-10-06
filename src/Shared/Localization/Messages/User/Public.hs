module Shared.Localization.Messages.User.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- User
_ERROR_SERVICE_USER__MISSING_USER =
  LocaleRecord "error.service.user.missing_user" "You have to be log in to perform this request." []

-- User Group
_ERROR_SERVICE_USER_GROUP__DUPLICATE_MEMBER =
  LocaleRecord "error.service.user_group.duplicate_member" "A user can be listed only once in the members of a user group" []
