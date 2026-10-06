module Shared.Localization.Messages.Settings.Public where

import Shared.Model.Localization.LocaleRecord

_ERROR_SERVICE_SETTINGS__ORGANIZATION_ONLY = LocaleRecord "error.service.settings.organization_only" "These settings exist only on the organization level, query parameter 'w' is not accepted" []

_ERROR_SERVICE_SETTINGS__OVERRIDE_NOT_ALLOWED = LocaleRecord "error.service.settings.override_not_allowed" "The organization does not allow workspaces to override these settings" []

_ERROR_SERVICE_SETTINGS__ROLE_NOT_IN_WORKSPACE = LocaleRecord "error.service.settings.role_not_in_workspace" "The default role must be a role of the workspace" []
