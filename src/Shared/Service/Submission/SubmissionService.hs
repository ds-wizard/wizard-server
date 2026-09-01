module Shared.Service.Submission.SubmissionService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Submission.Runner
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.UserSubmissionPropList
import Shared.S3.Document.DocumentS3
import Shared.Service.Common
import Shared.Service.Document.DocumentAcl
import Shared.Service.Submission.SubmissionAcl
import Shared.Service.Submission.SubmissionMapper
import Shared.Service.User.Profile.UserProfileService
import Shared.Service.User.WizardUserMapper (toSuggestion')
import Shared.Util.Uuid

getAvailableServicesForSubmission :: WizardRequestContextC s m => U.UUID -> m [TenantConfigSubmissionServiceSimple]
getAvailableServicesForSubmission docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentByUuid docUuid
  checkEditPermissionToSubmission doc
  findTenantConfigSubmissionServicesByDocumentTemplateUuidAndFormatUuid doc.documentTemplateUuid doc.formatUuid

getSubmissionsForDocument :: WizardRequestContextC s m => U.UUID -> m [SubmissionList]
getSubmissionsForDocument docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentByUuid docUuid
  checkViewPermissionToDoc doc.projectUuid
  findSubmissionsByDocumentUuid docUuid

submitDocument :: WizardRequestContextC s m => U.UUID -> SubmissionCreateDTO -> m SubmissionList
submitDocument docUuid reqDto =
  runInTransaction $ do
    checkIfSubmissionIsEnabled
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToSubmission doc
    tcSubmission <- findTenantConfigSubmissionServiceByServiceId reqDto.serviceId
    docContent <- retrieveDocumentContent docUuid
    userProps <- getUserProps tcSubmission
    sub <- createSubmission docUuid reqDto
    response <- uploadDocument tcSubmission.request userProps docContent
    let updatedSub =
          case response of
            Right mLocation ->
              sub
                { state = DoneSubmissionState
                , location = mLocation
                }
                :: Submission
            Left error ->
              sub
                { state = ErrorSubmissionState
                , returnedData = Just error
                }
                :: Submission
    savedSubmission <- updateSubmissionByUuid updatedSub
    currentUser <- getCurrentUser
    return $ toList savedSubmission tcSubmission (Just $ toSuggestion' currentUser)
  where
    getUserProps tcSubmission = do
      mUser <- asks (.currentUser')
      case mUser of
        Just user -> do
          submissionProps <- getUserProfileSubmissionProps user.uuid
          let mUserProps = L.find (\p -> p.sId == tcSubmission.sId) submissionProps
          return $
            case mUserProps of
              Just p -> p.values
              Nothing -> M.empty
        Nothing -> return M.empty

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled :: WizardRequestContextC s m => m ()
checkIfSubmissionIsEnabled = checkIfTenantFeatureIsEnabled "Submission" findTenantConfigSubmission (.enabled)

createSubmission :: WizardRequestContextC s m => U.UUID -> SubmissionCreateDTO -> m Submission
createSubmission docUuid reqDto = do
  sUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  currentUser <- getCurrentUser
  let sub = fromCreate sUuid reqDto.serviceId docUuid tenantUuid (Just currentUser.uuid) now
  insertSubmission sub
  return sub
