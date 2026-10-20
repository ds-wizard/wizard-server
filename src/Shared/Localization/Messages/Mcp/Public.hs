module Shared.Localization.Messages.Mcp.Public where

import Data.List (intercalate)

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
_ERROR_VALIDATION__MCP_CODE_CHALLENGE_METHOD method =
  LocaleRecord "error.validation.mcp_code_challenge_method" "Unsupported code challenge method ('%s'), only S256 is supported" [method]

_ERROR_VALIDATION__MCP_CODE_CHALLENGE =
  LocaleRecord "error.validation.mcp_code_challenge" "Code challenge must be 43 base64url characters" []

_ERROR_VALIDATION__MCP_MISSING_PERMISSION permission =
  LocaleRecord "error.validation.mcp_missing_permission" "Missing permission: %s" [permission]

_ERROR_VALIDATION__MCP_MISSING_ARGUMENT name =
  LocaleRecord "error.validation.mcp_missing_argument" "Missing required argument \"%s\"" [name]

_ERROR_VALIDATION__MCP_INVALID_ARGUMENT name details =
  LocaleRecord "error.validation.mcp_invalid_argument" "Invalid argument \"%s\": %s" [name, details]

_ERROR_VALIDATION__MCP_INVALID_CURSOR =
  LocaleRecord "error.validation.mcp_invalid_cursor" "Invalid argument \"cursor\": does not continue a result of this call" []

_ERROR_VALIDATION__MCP_WRITE_TOOL_CURSOR =
  LocaleRecord
    "error.validation.mcp_write_tool_cursor"
    "Invalid argument \"cursor\": the result of a tool that changes data cannot be continued; read the rest with a read tool"
    []

_ERROR_VALIDATION__MCP_ONE_OF_ARGUMENTS first second =
  LocaleRecord "error.validation.mcp_one_of_arguments" "Give exactly one of the arguments \"%s\" and \"%s\"" [first, second]

_ERROR_VALIDATION__MCP_NOT_BOTH_ARGUMENTS first second =
  LocaleRecord "error.validation.mcp_not_both_arguments" "Give %s or %s, not both" [first, second]

_ERROR_VALIDATION__MCP_DEPENDENT_ARGUMENT argument needed =
  LocaleRecord "error.validation.mcp_dependent_argument" "The argument \"%s\" needs \"%s\"" [argument, needed]

_ERROR_VALIDATION__MCP_RESOLUTION_EVENT_UUID =
  LocaleRecord
    "error.validation.mcp_resolution_event_uuid"
    "Missing required argument \"event_uuid\": apply and reject need the eventUuid of the current conflict"
    []

_ERROR_VALIDATION__MCP_SETTINGS_SECTION name names =
  LocaleRecord
    "error.validation.mcp_settings_section"
    "Invalid argument \"section\": unknown section %s, use one of %s"
    [name, intercalate ", " names]

_ERROR_VALIDATION__MCP_FILE_CONTENT_LENGTH path limit =
  LocaleRecord
    "error.validation.mcp_file_content_length"
    "Invalid argument \"files\": the content of %s is longer than %s characters"
    [path, show limit]

_ERROR_VALIDATION__MCP_FILES_CONTENT_LENGTH limit =
  LocaleRecord
    "error.validation.mcp_files_content_length"
    "Invalid argument \"files\": the content of all files is longer than %s characters"
    [show limit]

_ERROR_VALIDATION__MCP_COORDINATE reference =
  LocaleRecord "error.validation.mcp_coordinate" "Invalid argument \"id\": %s is not an id with a version" [show reference]

_ERROR_VALIDATION__MCP_BASE64 name =
  LocaleRecord "error.validation.mcp_base64" "Invalid argument \"%s\": not base64" [name]

_ERROR_VALIDATION__MCP_BASE64_SIZE name limit =
  LocaleRecord "error.validation.mcp_base64_size" "Invalid argument \"%s\": the decoded content is larger than %s MB" [name, show (limit `div` (1024 * 1024))]

_ERROR_VALIDATION__MCP_CONTENT_SIZE name limit =
  LocaleRecord "error.validation.mcp_content_size" "Invalid argument \"%s\": the content is larger than %s MB" [name, show (limit `div` (1024 * 1024))]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
_ERROR_SERVICE_MCP__INVALID_REQUEST =
  LocaleRecord "error.service.mcp.invalid_request" "Invalid request" []

_ERROR_SERVICE_MCP__METHOD_NOT_FOUND method =
  LocaleRecord "error.service.mcp.method_not_found" "Method not found: %s" [method]

_ERROR_SERVICE_MCP__MISSING_TOOL_NAME =
  LocaleRecord "error.service.mcp.missing_tool_name" "Missing tool name" []

_ERROR_SERVICE_MCP__UNKNOWN_TOOL name =
  LocaleRecord "error.service.mcp.unknown_tool" "Unknown tool: %s" [name]

_ERROR_SERVICE_MCP__TOOLSET_NOT_ENABLED toolset =
  LocaleRecord "error.service.mcp.toolset_not_enabled" "Toolset not enabled: %s" [toolset]

_ERROR_SERVICE_MCP__INTERNAL_ERROR =
  LocaleRecord "error.service.mcp.internal_error" "Internal error" []

_ERROR_SERVICE_MCP__CONFLICT message changes =
  LocaleRecord "error.service.mcp.conflict" "%s. Changed since: %s" [message, changes]

_ERROR_SERVICE_MCP__CHANGED_SINCE entityName updatedAt =
  LocaleRecord "error.service.mcp.changed_since" "The %s was changed at %s, after expected_updated_at; get its current state and repeat the change" [entityName, updatedAt]

_ERROR_SERVICE_MCP__MISSING_ARGUMENTS entityName arguments =
  LocaleRecord "error.service.mcp.missing_arguments" "A new %s needs the arguments %s" [entityName, intercalate ", " arguments]

_ERROR_SERVICE_MCP__INVALID_ARGUMENTS details =
  LocaleRecord "error.service.mcp.invalid_arguments" "Invalid arguments: %s" [details]

_ERROR_SERVICE_MCP__UNCHANGEABLE_ARGUMENT argument entityName =
  LocaleRecord "error.service.mcp.unchangeable_argument" "The argument %s of an existing %s cannot be changed" [argument, entityName]

_ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH =
  LocaleRecord
    "error.service.mcp.confirmation_token_mismatch"
    "The confirmation_token does not belong to this call, or what it confirms has changed since; call the tool again without it to get a new summary and token"
    []

_ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_EXPIRED =
  LocaleRecord "error.service.mcp.confirmation_token_expired" "The confirmation_token has expired; call the tool again without it to get a new summary and token" []

_ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_USED =
  LocaleRecord "error.service.mcp.confirmation_token_used" "The confirmation_token was used already; call the tool again without it to get a new summary and token" []

_ERROR_SERVICE_MCP__MIGRATION_NOT_COMPLETED =
  LocaleRecord "error.service.mcp.migration_not_completed" "The migration of the knowledge model editor is not completed; resolve its conflicts first" []

_ERROR_SERVICE_MCP__DOCUMENT_CONTEXT_PATH_NOT_FOUND path =
  LocaleRecord "error.service.mcp.document_context_path_not_found" "The document context has nothing at %s" [path]

_ERROR_SERVICE_MCP__NOT_A_DRAFT dtUuid =
  LocaleRecord "error.service.mcp.not_a_draft" "The document template %s is not a draft; create a draft based on it with save_document_template_draft and change that one" [dtUuid]

_ERROR_SERVICE_MCP__FILE_NOT_FOUND path =
  LocaleRecord "error.service.mcp.file_not_found" "The template has no file %s; give content to create it" [path]

_ERROR_SERVICE_MCP__ASSET_NOT_FOUND path =
  LocaleRecord "error.service.mcp.asset_not_found" "The template has no asset %s; give content and content_type to create it" [path]

_ERROR_SERVICE_MCP__FOLDER_WRITE path =
  LocaleRecord "error.service.mcp.folder_write" "%s is a folder: give only new_path to move it" [path]

_ERROR_SERVICE_MCP__PREVIEW_LIMIT limit =
  LocaleRecord
    "error.service.mcp.preview_limit"
    "The organization already has %s previews being generated; wait a minute and try again, or poll a pending preview with its document_uuid"
    [show limit]

_ERROR_SERVICE_MCP__DRAFT_NOT_RELEASED dtUuid =
  LocaleRecord "error.service.mcp.draft_not_released" "The document template %s is a draft; release it with publish_document_template_draft" [dtUuid]

_ERROR_SERVICE_MCP__SETTINGS_NOT_OVERRIDABLE section =
  LocaleRecord "error.service.mcp.settings_not_overridable" "The %s section has no workspace overrides to reset" [section]

_ERROR_SERVICE_MCP__API_KEY_EXPIRATION maxDays =
  LocaleRecord "error.service.mcp.api_key_expiration" "expires_at of an API key must lie in the future and at most %s days from now" [show maxDays]

_ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN =
  LocaleRecord
    "error.service.mcp.last_admin_login"
    "No active admin could log in any more: none of them has a password or a single sign-on that stays; give an admin a password first (set_user_password)"
    []

_ERROR_SERVICE_MCP__MASKED_SECRET_TARGET_CHANGED target =
  LocaleRecord
    "error.service.mcp.masked_secret_target_changed"
    "The %s changes in this call, so ******** cannot keep the stored secret: give the secret values again"
    [target]

_ERROR_SERVICE_MCP__OUTPUT_TOO_LARGE =
  LocaleRecord
    "error.service.mcp.output_too_large"
    "The result does not fit into the size limit of the tool output and cannot be cut; narrow the call. A tool that changes data has made its change nevertheless"
    []

_ERROR_SERVICE_MCP__DOCUMENT_NOT_DONE documentUuid state =
  LocaleRecord
    "error.service.mcp.document_not_done"
    "The document %s has no content (%s): wait for a pending one with create_document and its document_uuid; a failed one has a workerLog"
    [documentUuid, state]

_ERROR_SERVICE_MCP__DEV_OPERATION_ABSENCE section operation =
  LocaleRecord "error.service.mcp.dev_operation_absence" "There is no dev operation '%s' in the section '%s'; list_dev_operations lists them" [operation, section]

_ERROR_SERVICE_MCP__DEV_OPERATION_PARAMETERS operation parameters =
  LocaleRecord "error.service.mcp.dev_operation_parameters" "The dev operation '%s' needs the parameters %s" [operation, intercalate ", " parameters]

_ERROR_SERVICE_MCP__PERSISTENT_COMMAND_DONE uuid =
  LocaleRecord "error.service.mcp.persistent_command_done" "The persistent command %s is done; only a new, failed or ignored command can be retried" [uuid]
