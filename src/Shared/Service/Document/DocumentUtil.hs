module Shared.Service.Document.DocumentUtil where

import Data.Foldable (toList)
import Data.Hashable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Control.Monad.Reader (asks)
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Settings.SettingsUsersDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Model.Common.Page
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Service.Document.DocumentMapper
import Shared.Service.Settings.SettingsService
import Shared.Service.Settings.WorkspaceSettingsService

enhanceDocuments :: WizardRequestContextC s m => Page DocumentList -> m (Page DocumentDTO)
enhanceDocuments docPage = do
  let workspaceUuids = L.nub (fmap (.workspaceUuid) (toList docPage))
  submissionSettings <- M.fromList <$> traverse (\workspaceUuid -> (,) workspaceUuid <$> getEffectiveSettingsSubmission (Just workspaceUuid)) workspaceUuids
  traverse (enhanceDocument submissionSettings) docPage

enhanceDocument :: WizardRequestContextC s m => M.Map U.UUID SettingsSubmission -> DocumentList -> m DocumentDTO
enhanceDocument submissionSettings doc = do
  submissions <-
    if maybe False (.enabled) (M.lookup doc.workspaceUuid submissionSettings)
      then findSubmissionsByDocumentUuid doc.uuid
      else return []
  return $ toDTO doc submissions

filterAlreadyDoneDocument :: U.UUID -> U.UUID -> Maybe String -> Document -> Bool
filterAlreadyDoneDocument documentTemplateUuid formatUuid mLanguage doc =
  (doc.state == DoneDocumentState || doc.state == ErrorDocumentState) && Just doc.documentTemplateUuid == Just documentTemplateUuid && Just doc.formatUuid == Just formatUuid && doc.language == mLanguage

computeHash :: [KnowledgeModelEvent] -> Project -> [ProjectVersion] -> Maybe U.UUID -> M.Map String Reply -> DocumentContextOrganization -> Maybe UserDTO -> Int
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

getDocumentContextOrganization :: WizardRequestContextC s m => m DocumentContextOrganization
getDocumentContextOrganization = do
  tenantUuid <- asks (.tenantUuid')
  tenant <- findTenantByUuid tenantUuid
  users <- getSettingsByTenantUuid findSettingsUsers tenantUuid
  return $ DocumentContextOrganization {name = tenant.name, affiliations = users.affiliations}
