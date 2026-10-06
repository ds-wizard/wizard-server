module Shared.Service.DocumentTemplate.DocumentTemplateAcl where

import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.Acl.LibraryAcl

checkEditorPermissionToDocumentTemplate :: WizardRequestContextC s m => U.UUID -> m ()
checkEditorPermissionToDocumentTemplate dtUuid = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  dt <- findDocumentTemplateByUuid dtUuid
  checkPermissionInWorkspace _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION dt.workspaceUuid

checkManagePermissionToDocumentTemplate :: WizardRequestContextC s m => U.UUID -> m ()
checkManagePermissionToDocumentTemplate dtUuid = do
  checkLibraryPermissionAnywhere _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
  dt <- findDocumentTemplateByUuid dtUuid
  checkLibraryPermission _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION dt.workspaceUuid
