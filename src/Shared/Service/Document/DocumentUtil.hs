module Shared.Service.Document.DocumentUtil where

import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Document.DocumentMapper
import Shared.Service.Submission.SubmissionService

enhanceDocument :: WizardRequestContextC s m => DocumentList -> m DocumentDTO
enhanceDocument doc = do
  tcSubmission <- findTenantConfigSubmission
  submissions <-
    if tcSubmission.enabled
      then getSubmissionsForDocument doc.uuid
      else return []
  return $ toDTO doc submissions

filterAlreadyDoneDocument :: U.UUID -> U.UUID -> Maybe String -> Document -> Bool
filterAlreadyDoneDocument documentTemplateUuid formatUuid mLanguage doc =
  (doc.state == DoneDocumentState || doc.state == ErrorDocumentState) && Just doc.documentTemplateUuid == Just documentTemplateUuid && Just doc.formatUuid == Just formatUuid && doc.language == mLanguage

computeHash :: [KnowledgeModelEvent] -> Project -> [ProjectVersion] -> Maybe U.UUID -> M.Map String Reply -> TenantConfigOrganization -> Maybe UserDTO -> Int
computeHash kmEditorEvents project versions phaseUuid replies tcOrganization mCurrentUser =
  sum
    [ hash kmEditorEvents
    , hash project.name
    , hash project.description
    , hash versions
    , hash project.projectTags
    , maybe 0 hash phaseUuid
    , hash . M.toList $ replies
    , hash tcOrganization
    , maybe 0 hash mCurrentUser
    ]
