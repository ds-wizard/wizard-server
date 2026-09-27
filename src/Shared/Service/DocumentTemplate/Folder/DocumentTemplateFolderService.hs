module Shared.Service.DocumentTemplate.Folder.DocumentTemplateFolderService where

import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Service.DocumentTemplate.DocumentTemplateAcl

moveDraftFolder :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFolderMoveDTO -> m ()
moveDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate documentTemplateUuid
    moveFolder documentTemplateUuid reqDto.current reqDto.new
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()

deleteDraftFolder :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFolderDeleteDTO -> m ()
deleteDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate documentTemplateUuid
    deleteFolder documentTemplateUuid reqDto.path
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()
