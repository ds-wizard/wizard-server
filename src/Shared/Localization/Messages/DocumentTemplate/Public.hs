module Shared.Localization.Messages.DocumentTemplate.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Uniqueness
_ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS tmlId =
  LocaleRecord "error.validation.tml_id_uniqueness" "DocumentTemplate '%s' already exists" [tmlId]

-- Document Template Locale
_ERROR_VALIDATION__DOC_TML_LOCALE_INVALID_PO reason =
  LocaleRecord "error.validation.doc_tml_locale_invalid_po" "Unable to parse the PO file: %s" [reason]

_ERROR_VALIDATION__DOC_TML_LOCALE_MISSING_LANGUAGE =
  LocaleRecord "error.validation.doc_tml_locale_missing_language" "The PO file has no 'Language' header field" []

_ERROR_VALIDATION__DOC_TML_LOCALE_CODE_UNIQUENESS code =
  LocaleRecord "error.validation.doc_tml_locale_code_uniqueness" "Translation for language '%s' already exists" [code]

_ERROR_VALIDATION__DOC_TML_LOCALE_NOT_AVAILABLE code =
  LocaleRecord "error.validation.doc_tml_locale_not_available" "Document template has no translation for language '%s'" [code]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- DocumentTemplate
_ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON =
  LocaleRecord
    "error.service.tb.missing_template_json"
    "Desired definition ('template.json') wasn't found in archive"
    []

_ERROR_SERVICE_TB__MISSING_ASSET fileName =
  LocaleRecord "error.service.tb.missing_asset" "Desired asset ('%s') wasn't found in zip" [fileName]

_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON errorMessage =
  LocaleRecord
    "error.service.tb.unable_to_decode_template_json"
    "Desired definition ('template.json') couldn't be decoded (error: '%s')"
    [errorMessage]

_ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tmlId tmlMetamodelVersion appTmlMetamodelVersion =
  LocaleRecord
    "error.validation.tml_unsupported_metamodel_version"
    "DocumentTemplate '%s' contains unsupported version of metamodel (template metamodel version: '%s', application metamodel version: '%s')"
    [tmlId, tmlMetamodelVersion, appTmlMetamodelVersion]

_ERROR_SERVICE_DOC_TML__POT_FILE_NOT_READY =
  LocaleRecord
    "error.service.doc_tml.pot_file_not_ready"
    "The POT file for this document template has not been generated yet"
    []
