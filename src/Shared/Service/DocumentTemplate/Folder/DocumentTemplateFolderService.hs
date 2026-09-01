module Shared.Service.DocumentTemplate.Folder.DocumentTemplateFolderService where

import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext

moveDraftFolder :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFolderMoveDTO -> m ()
moveDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    moveFolder documentTemplateUuid reqDto.current reqDto.new
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()

deleteDraftFolder :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFolderDeleteDTO -> m ()
deleteDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    deleteFolder documentTemplateUuid reqDto.path
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()
