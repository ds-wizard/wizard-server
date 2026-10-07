module Shared.Localization.Messages.Locale.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Locale
_ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE =
  LocaleRecord "error.validation.deactivate_default_locale" "You can't deactivate default locale" []

_ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT =
  LocaleRecord "error.validation.locale_disabled_default" "You can't set disabled locale as default" []

-- Upload
_ERROR_VALIDATION__LOCALE_PO_UNPARSABLE error =
  LocaleRecord "error.validation.locale_po_unparsable" "The PO file can't be parsed (error: '%s')" [error]

_ERROR_VALIDATION__LOCALE_PO_MISSING_LANGUAGE =
  LocaleRecord "error.validation.locale_po_missing_language" "The PO file has no 'Language' header" []

_ERROR_VALIDATION__LOCALE_PO_INVALID_LANGUAGE language =
  LocaleRecord "error.validation.locale_po_invalid_language" "The PO file has an invalid 'Language' header ('%s')" [language]

_ERROR_VALIDATION__LOCALE_PO_TOO_LARGE =
  LocaleRecord "error.validation.locale_po_too_large" "The PO file is larger than 5 MB" []

_ERROR_VALIDATION__LOCALE_PO_UNSUPPORTED_KIND kind =
  LocaleRecord
    "error.validation.locale_po_unsupported_kind"
    "The PO file was made for '%s', which is not a translation of the application UI"
    [kind]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Mail Template Locale
_ERROR_SERVICE_MAIL_TEMPLATE_LOCALE__POT_NOT_AVAILABLE =
  LocaleRecord "error.service.mail_template_locale.pot_not_available" "The mail template POT is not available yet" []
