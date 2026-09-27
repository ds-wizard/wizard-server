module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAcl where

import qualified Data.UUID as U

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.Acl.LibraryAcl

checkManagePermissionToPackage :: WizardRequestContextC s m => U.UUID -> m ()
checkManagePermissionToPackage pkgUuid = do
  checkLibraryPermissionAnywhere _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  pkg <- findPackageByUuid pkgUuid
  checkLibraryPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION pkg.workspaceUuid
