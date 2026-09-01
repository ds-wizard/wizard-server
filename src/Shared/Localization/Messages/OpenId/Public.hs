module Shared.Localization.Messages.OpenId.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Open ID
_ERROR_VALIDATION__OPENID_WRONG_RESPONSE error =
  LocaleRecord "error.validation.openid_wrong_response" "There was a wrong response from OpenID (error: '%s')" [error]

_ERROR_VALIDATION__OPENID_CODE_ABSENCE =
  LocaleRecord "error.validation.openid_code_absence" "Auth Code is not provided" []

-- OpenId
_ERROR_SERVICE_OPENID__UNABLE_TO_ENCODE_JWT_TOKEN error =
  LocaleRecord "error.service.auth.unable_to_encode_jwt_token" "Unable to encode JWT token (error: %s)" [error]

_ERROR_SERVICE_OPENID__REGISTRATION_DISABLED =
  LocaleRecord "error.service.openid.registration_disabled" "Registration of new accounts via this service is disabled" []

_ERROR_SERVICE_OPENID__IDENTITY_LINKED_TO_DIFFERENT_USER =
  LocaleRecord "error.service.openid.identity_linked_to_different_user" "This external identity is already linked to a different user" []
