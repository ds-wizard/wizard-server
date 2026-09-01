module Shared.Service.Project.File.ProjectFileService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.File.FileCreateDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.Project
import Shared.S3.Project.ProjectFileS3
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Project.File.ProjectFileAcl
import Shared.Service.Project.File.ProjectFileMapper
import Shared.Service.Project.File.ProjectFileValidation
import Shared.Service.Project.ProjectAcl
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Util.String
import Shared.Util.Uuid

getProjectFilesPage :: WizardRequestContextC s m => Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> m (Page ProjectFileList)
getProjectFilesPage mQuery mProjectUuid pageable sort = do
  case mProjectUuid of
    Just projectUuid -> do
      project <- findProjectByUuid projectUuid
      checkViewPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> checkPermission _PROJECTS_EDIT_ROLE_PERMISSION
  findProjectFilesPage mQuery mProjectUuid pageable sort

createProjectFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> FileCreateDTO -> m ProjectFileList
createProjectFile projectUuid questionUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkViewPermissionToProject project.visibility project.sharing project.permissions
    uuid <- liftIO generateUuid
    mCurrentUser <- asks (.currentUser')
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let projectFile = fromFileCreateDTO reqDto uuid projectUuid mCurrentUser tenantUuid now
    validateProjectFile project questionUuid projectFile
    insertProjectFile projectFile
    putFile projectUuid uuid reqDto.contentType reqDto.content
    addFile projectUuid (toSimple projectFile)
    return $ toList projectFile project mCurrentUser

cloneProjectFiles :: WizardRequestContextC s m => U.UUID -> U.UUID -> m [(ProjectFile, ProjectFile)]
cloneProjectFiles oldProjectUuid newProjectUuid = do
  runInTransaction $ do
    oldFiles <- findProjectFilesByProject oldProjectUuid
    traverse
      ( \oldFile -> do
          contentAction <- retrieveFileConduitAction oldProjectUuid oldFile.uuid
          newFileUuid <- liftIO generateUuid
          let newFile = oldFile {uuid = newFileUuid, projectUuid = newProjectUuid}
          let contentDisposition = f' "attachment;filename=\"%s\"" [trim newFile.fileName]
          insertProjectFile newFile
          putFileConduit newProjectUuid newFile.uuid newFile.contentType contentDisposition contentAction
          return (oldFile, newFile)
      )
      oldFiles

downloadProjectFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> m TemporaryFileDTO
downloadProjectFile projectUuid fileUuid = do
  runInTransaction $ do
    projectFile <- findProjectFileByUuid fileUuid
    checkViewPermissionToFile projectUuid
    contentAction <- retrieveFileConduitAction projectUuid fileUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFileConduit projectFile.fileName "application/octet-stream" mCurrentUserUuid contentAction
    return $ TemporaryFileMapper.toDTO url projectFile.contentType

deleteProjectFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteProjectFile projectUuid fileUuid = do
  runInTransaction $ do
    _ <- findProjectFileByUuid fileUuid
    checkEditPermissionToFile projectUuid
    void $ deleteProjectFileByUuid fileUuid
    void $ deleteProjectCacheByProjectUuid projectUuid
