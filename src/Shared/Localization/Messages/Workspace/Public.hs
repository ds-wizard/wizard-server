module Shared.Localization.Messages.Workspace.Public where

import Shared.Model.Localization.LocaleRecord

_ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT = LocaleRecord "error.service.workspace.scope_conflict" "Query parameters 'w' and 'tenant' can not be combined" []

_ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED = LocaleRecord "error.service.workspace.scope_required" "Query parameter 'w' or 'tenant' is required" []

_ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED = LocaleRecord "error.service.workspace.workspace_required" "Query parameter 'w' is required" []

_ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED = LocaleRecord "error.service.workspace.workspace_not_accepted" "Query parameter 'w' is not accepted in a single-workspace tenant" []

_ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED = LocaleRecord "error.service.workspace.tenant_not_accepted" "Query parameter 'tenant' is not accepted for this entity" []

_ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT = LocaleRecord "error.service.workspace.single_workspace_tenant" "The tenant does not allow more than one workspace" []

_ERROR_SERVICE_WORKSPACE__LAST_WORKSPACE = LocaleRecord "error.service.workspace.last_workspace" "The last workspace of a tenant can not be deleted" []

_ERROR_SERVICE_WORKSPACE__MULTIPLE_WORKSPACES = LocaleRecord "error.service.workspace.multiple_workspaces" "The tenant has more than one workspace" []

_ERROR_SERVICE_WORKSPACE__MULTI_WORKSPACE_TENANT = LocaleRecord "error.service.workspace.multi_workspace_tenant" "The operation is not available in a multi-workspace tenant" []

_ERROR_SERVICE_WORKSPACE__SYSTEM_USER = LocaleRecord "error.service.workspace.system_user" "The membership of the system user can not be changed" []

_ERROR_SERVICE_WORKSPACE__NOT_MEMBER = LocaleRecord "error.service.workspace.not_member" "The user is not a member of the workspace" []

_ERROR_SERVICE_WORKSPACE__MULTI_WORKSPACE_IRREVERSIBLE = LocaleRecord "error.service.workspace.multi_workspace_irreversible" "A multi-workspace tenant can not be switched back to a single workspace" []

_ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE = LocaleRecord "error.service.workspace.workspace_plane_unavailable" "The workspace level of roles, memberships and settings is not available in a single-workspace tenant" []
