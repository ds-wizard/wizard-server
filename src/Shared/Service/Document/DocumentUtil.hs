module Shared.Service.Document.DocumentUtil where

import Data.Foldable (toList)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Model.Common.Page
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentList
import Shared.Model.Settings.Settings
import Shared.Service.Document.DocumentMapper
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

matchesDocumentKey :: String -> U.UUID -> U.UUID -> Maybe String -> Document -> Bool
matchesDocumentKey name documentTemplateUuid formatUuid mLanguage doc =
  doc.name == name
    && doc.documentTemplateUuid == documentTemplateUuid
    && doc.formatUuid == formatUuid
    && doc.language == mLanguage

isFinishedDocument :: Document -> Bool
isFinishedDocument doc = doc.state == DoneDocumentState || doc.state == ErrorDocumentState

isPendingDocument :: Document -> Bool
isPendingDocument doc = doc.state == QueuedDocumentState || doc.state == InProgressDocumentState
