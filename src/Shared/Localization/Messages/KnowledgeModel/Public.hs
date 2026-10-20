module Shared.Localization.Messages.KnowledgeModel.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Absence
_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Knowledge Model Bundle doesn't contain main package" []

-- Uniqueness
_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

-- Knowledge Model Locale
_ERROR_VALIDATION__KM_LOCALE_INVALID_PO reason =
  LocaleRecord "error.validation.km_locale_invalid_po" "Unable to parse the PO file: %s" [reason]

_ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE =
  LocaleRecord "error.validation.km_locale_missing_language" "The PO file has no 'Language' header field" []

_ERROR_VALIDATION__KM_LOCALE_INVALID_JSON reason =
  LocaleRecord "error.validation.km_locale_invalid_json" "Unable to parse the JSON translation file: %s" [reason]

_ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS code =
  LocaleRecord "error.validation.km_locale_code_uniqueness" "Translation for language '%s' already exists" [code]

_ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE =
  LocaleRecord "error.validation.km_locale_not_reusable" "Selected locales cannot be reused from the previous package version" []

-- Knowledge Model Event
_ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE uuid entityKind =
  LocaleRecord "error.validation.km_event_entity_absence" "'%s' is not an existing %s" [uuid, entityKind]

_ERROR_VALIDATION__KM_EVENT_INVALID_UUID value =
  LocaleRecord "error.validation.km_event_invalid_uuid" "'%s' is neither a UUID nor a symbolic name" [value]

_ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME name =
  LocaleRecord "error.validation.km_event_undefined_name" "'%s' is not defined by the entity UUID of a preceding Add event" [name]

_ERROR_VALIDATION__KM_EVENT_INVALID_CONTENT reason =
  LocaleRecord "error.validation.km_event_invalid_content" "The event content cannot be read: %s" [reason]

_ERROR_VALIDATION__KM_EVENT_ENTITY_UNIQUENESS uuid =
  LocaleRecord "error.validation.km_event_entity_uniqueness" "Entity '%s' already exists" [uuid]

_ERROR_VALIDATION__KM_EVENT_PARENT_MISMATCH uuid parentUuid =
  LocaleRecord "error.validation.km_event_parent_mismatch" "The parent of '%s' is '%s'" [uuid, parentUuid]

_ERROR_VALIDATION__KM_EVENT_MOVE_CYCLE uuid targetUuid =
  LocaleRecord "error.validation.km_event_move_cycle" "'%s' cannot be moved under '%s', which is the entity itself or its descendant" [uuid, targetUuid]

_ERROR_VALIDATION__KM_EVENT_CHILDREN_PERMUTATION childUuids =
  LocaleRecord "error.validation.km_event_children_permutation" "The list has to be a reordering of the current children: [%s]" [childUuids]

_ERROR_VALIDATION__KM_EVENT_EMPTY_TEXT =
  LocaleRecord "error.validation.km_event_empty_text" "The text must not be empty" []

_ERROR_VALIDATION__KM_EVENT_INVALID_URL url =
  LocaleRecord "error.validation.km_event_invalid_url" "'%s' is not a valid URL" [url]

_ERROR_VALIDATION__KM_EVENT_INVALID_EMAIL email =
  LocaleRecord "error.validation.km_event_invalid_email" "'%s' is not a valid e-mail address" [email]

_ERROR_VALIDATION__KM_EVENT_INVALID_REGEX regex =
  LocaleRecord "error.validation.km_event_invalid_regex" "'%s' is not a valid regular expression" [regex]

_ERROR_VALIDATION__KM_EVENT_INVERTED_RANGE lowerLimit upperLimit =
  LocaleRecord "error.validation.km_event_inverted_range" "The lower limit '%s' is greater than the upper limit '%s'" [lowerLimit, upperLimit]

_ERROR_VALIDATION__KM_EVENT_OUT_OF_UNIT_RANGE value =
  LocaleRecord "error.validation.km_event_out_of_unit_range" "'%s' is not between 0 and 1" [value]

_ERROR_VALIDATION__KM_EVENT_VALIDATION_VALUE_TYPE validation valueType =
  LocaleRecord "error.validation.km_event_validation_value_type" "'%s' cannot be used for a question of value type '%s'" [validation, valueType]

_ERROR_VALIDATION__KM_EVENT_REJECTED eventUuid =
  LocaleRecord "error.validation.km_event_rejected" "The event '%s' was rejected; reload the editor" [eventUuid]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Document Template
_ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML =
  LocaleRecord
    "error.service.doc_tml.non_editable_doc_tml"
    "Unable to export / edit non editable document template"
    []

-- Knowledge Model Editor
_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__OUTDATED_BASE_REVISION baseRevision =
  LocaleRecord
    "error.service.knowledge_model_editor.outdated_base_revision"
    "The editor has changed since revision '%s'"
    [baseRevision]

_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__UNKNOWN_BASE_REVISION baseRevision =
  LocaleRecord
    "error.service.knowledge_model_editor.unknown_base_revision"
    "The revision '%s' is not an event of the editor (it may have been squashed); read the current state of the editor and send the events again"
    [baseRevision]

_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__MIGRATION_IN_PROGRESS =
  LocaleRecord
    "error.service.knowledge_model_editor.migration_in_progress"
    "The editor has a knowledge model migration in progress; finish or cancel the migration before changing the editor"
    []

-- Package
_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []

_ERROR_SERVICE_PKG__NON_EDITABLE_PKG =
  LocaleRecord
    "error.service.pkg.non_editable_pkg"
    "Unable to export / edit non editable package"
    []
