module Shared.Service.Submission.SubmissionService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Submission.Runner
import Shared.Localization.Messages.Public
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.User.UserSubmissionPropList
import Shared.S3.Document.DocumentS3
import Shared.Service.Common
import Shared.Service.Document.DocumentAcl
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Submission.SubmissionAcl
import Shared.Service.Submission.SubmissionMapper
import Shared.Service.User.Profile.UserProfileService
import Shared.Service.User.WizardUserMapper (toSuggestion')
import Shared.Util.Uuid

getAvailableServicesForSubmission :: WizardRequestContextC s m => U.UUID -> m [SettingsSubmissionServiceSimple]
getAvailableServicesForSubmission docUuid = do
  doc <- findDocumentByUuid docUuid
  checkIfSubmissionIsEnabled doc
  checkEditPermissionToSubmission doc
  fmap toSubmissionServiceSimple <$> findServicesForDocument doc

getSubmissionsForDocument :: WizardRequestContextC s m => U.UUID -> m [SubmissionList]
getSubmissionsForDocument docUuid = do
  doc <- findDocumentByUuid docUuid
  checkIfSubmissionIsEnabled doc
  checkViewPermissionToDoc doc.projectUuid
  findSubmissionsByDocumentUuid docUuid

submitDocument :: WizardRequestContextC s m => U.UUID -> SubmissionCreateDTO -> m SubmissionList
submitDocument docUuid reqDto =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkIfSubmissionIsEnabled doc
    checkEditPermissionToSubmission doc
    services <- findServicesForDocument doc
    service <- maybe (throwError . NotExistsError $ _ERROR_VALIDATION__ABSENCE "submission service") return (L.find (\s -> s.sId == reqDto.serviceId) services)
    docContent <- retrieveDocumentContent docUuid
    userProps <- getUserProps service
    sub <- createSubmission docUuid reqDto
    response <- uploadDocument service.request userProps docContent
    let updatedSub =
          case response of
            Right mLocation -> sub {state = DoneSubmissionState, location = mLocation} :: Submission
            Left error -> sub {state = ErrorSubmissionState, returnedData = Just error} :: Submission
    savedSubmission <- updateSubmissionByUuid updatedSub
    currentUser <- getCurrentUser
    return $ toList savedSubmission service (Just $ toSuggestion' currentUser)
  where
    getUserProps service = do
      mUser <- asks (.currentUser')
      case mUser of
        Just user -> do
          submissionProps <- getUserProfileSubmissionProps user.uuid
          return . maybe M.empty (.values) $ L.find (\p -> p.sId == service.sId) submissionProps
        Nothing -> return M.empty

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled :: WizardRequestContextC s m => Document -> m ()
checkIfSubmissionIsEnabled doc = checkIfTenantFeatureIsEnabled "Submission" (getEffectiveSettingsSubmission (Just doc.workspaceUuid)) (.enabled)

findServicesForDocument :: WizardRequestContextC s m => Document -> m [SettingsSubmissionService]
findServicesForDocument doc = do
  settings <- getEffectiveSettingsSubmission (Just doc.workspaceUuid)
  template <- findDocumentTemplateByUuid doc.documentTemplateUuid
  format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid doc.documentTemplateUuid doc.formatUuid
  let supported f = f.id == template.id && f.version == template.version && f.formatName == format.name
  return $ filter (any supported . (.supportedFormats)) settings.services

toSubmissionServiceSimple :: SettingsSubmissionService -> SettingsSubmissionServiceSimple
toSubmissionServiceSimple service = SettingsSubmissionServiceSimple {sId = service.sId, name = service.name, description = service.description}

createSubmission :: WizardRequestContextC s m => U.UUID -> SubmissionCreateDTO -> m Submission
createSubmission docUuid reqDto = do
  sUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  currentUser <- getCurrentUser
  let sub = fromCreate sUuid reqDto.serviceId docUuid tenantUuid (Just currentUser.uuid) now
  insertSubmission sub
  return sub
