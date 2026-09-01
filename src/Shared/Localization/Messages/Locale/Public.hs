module Shared.Localization.Messages.Locale.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Delete
_ERROR_VALIDATION__DEFAULT_LOCALE_DELETION =
  LocaleRecord "error.validation.default_locale_deletion" "You can't delete default locale" []

_ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETION =
  LocaleRecord "error.validation.default_locale_deletion" "You can't delete default wizard locale" []

-- Uniqueness
_ERROR_VALIDATION__LCL_ID_UNIQUENESS lclId =
  LocaleRecord "error.validation.lcl_id_uniqueness" "Locale '%s' already exists" [lclId]

-- Locale
_ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE =
  LocaleRecord "error.validation.deactivate_default_locale" "You can't deactivate default locale" []

_ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT =
  LocaleRecord "error.validation.locale_disabled_default" "You can't set disabled locale as default" []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Locale
_ERROR_SERVICE_LB__MISSING_LOCALE_JSON =
  LocaleRecord
    "error.service.lb.missing_locale_json"
    "Desired definition ('locale.json') wasn't found in archive"
    []

_ERROR_SERVICE_LB__UNABLE_TO_DECODE_LOCALE_JSON errorMessage =
  LocaleRecord
    "error.service.lb.unable_to_decode_locale_json"
    "Desired definition ('locale.json') couldn't be decoded (error: '%s')"
    [errorMessage]

_ERROR_SERVICE_LB__MISSING_FILE fileName =
  LocaleRecord "error.service.lb.missing_file" "Desired file ('%s') wasn't found in zip" [fileName]
