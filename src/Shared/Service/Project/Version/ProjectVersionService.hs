module Shared.Service.Project.Version.ProjectVersionService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.ProjectContentDTO
import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Lens
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Project.Comment.ProjectCommentService
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.ProjectAcl
import Shared.Service.Project.ProjectMapper
import Shared.Service.Project.Version.ProjectVersionMapper
import Shared.Service.Project.Version.ProjectVersionValidation
import Shared.Service.User.UserService
import Shared.Util.List
import Shared.Util.Uuid

getVersions :: WizardRequestContextC s m => U.UUID -> m [ProjectVersionList]
getVersions projectUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  findProjectVersionListByProjectUuidAndCreatedAt projectUuid Nothing

createVersion :: WizardRequestContextC s m => U.UUID -> ProjectVersionChangeDTO -> m ProjectVersionList
createVersion projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    validateProjectVersionCreate projectUuid reqDto
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let version = fromVersionChangeDTO reqDto uuid projectUuid tenantUuid currentUser.uuid now
    insertProjectVersion version
    return $ toVersionList version (Just currentUser)

cloneProjectVersions :: WizardRequestContextC s m => U.UUID -> U.UUID -> [(U.UUID, ProjectEventList)] -> m [(ProjectVersion, ProjectVersion)]
cloneProjectVersions oldProjectUuid newProjectUuid newProjectEventsWithOldEventUuid = do
  runInTransaction $ do
    oldVersions <- findProjectVersionsByProjectUuid oldProjectUuid
    traverse
      ( \oldVersion -> do
          newVersionUuid <- liftIO generateUuid
          let newEventUuid =
                case L.find (\(oldEventUuid, newEvent) -> oldVersion.eventUuid == oldEventUuid) newProjectEventsWithOldEventUuid of
                  Just (_, newEvent) -> getUuid newEvent
                  Nothing -> oldVersion.eventUuid
          let newVersion = oldVersion {uuid = newVersionUuid, projectUuid = newProjectUuid, eventUuid = newEventUuid}
          insertProjectVersion newVersion
          return (oldVersion, newVersion)
      )
      oldVersions

modifyVersion :: WizardRequestContextC s m => U.UUID -> U.UUID -> ProjectVersionChangeDTO -> m ProjectVersionList
modifyVersion projectUuid versionUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    validateProjectVersionUpdate reqDto
    now <- liftIO getCurrentTime
    version <- findProjectVersionByUuid versionUuid
    let updatedVersion = fromVersionChangeDTO' version reqDto now
    updateProjectVersionByUuid updatedVersion
    createdBy <-
      case version.createdBy of
        Just vCreatedBy -> do
          user <- getUserById vCreatedBy
          return . Just $ user
        Nothing -> return Nothing
    return $ toVersionList updatedVersion createdBy

deleteVersion :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteVersion projectUuid vUuid =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    _ <- findProjectVersionByUuid vUuid
    void $ deleteProjectVersionByUuid vUuid
    void $ deleteProjectCacheByProjectUuid projectUuid

revertToEvent :: WizardRequestContextC s m => U.UUID -> ProjectVersionRevertDTO -> Bool -> m ProjectContentDTO
revertToEvent projectUuid reqDto shouldSave =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    if shouldSave
      then checkOwnerPermissionToProject project.visibility project.permissions
      else checkViewPermissionToProject project.visibility project.sharing project.permissions
    projectVersions <- findProjectVersionsByProjectUuid projectUuid
    projectEvents <- findProjectEventListsByProjectUuid projectUuid
    let updatedEvents = takeWhileInclusive (\e -> getUuid e /= reqDto.eventUuid) projectEvents
    let eventsToDelete = dropWhileExclusive (\e -> getUuid e /= reqDto.eventUuid) projectEvents
    let updatedEventUuids = S.fromList . fmap getUuid $ updatedEvents
    let updatedVersions = filter (\v -> S.member v.eventUuid updatedEventUuids) projectVersions
    when
      shouldSave
      ( do
          let versionsToDelete = fmap (.uuid) . filter (\v -> not $ S.member v.eventUuid updatedEventUuids) $ projectVersions
          deleteProjectVersionsByUuids versionsToDelete
          deleteProjectEventsByUuids (fmap getUuid eventsToDelete)
          event <- findProjectEventByUuid reqDto.eventUuid
          deleteProjectFilesNewerThen projectUuid (getCreatedAt event)
          void $ updateProjectUpdatedAtByUuid projectUuid
      )
    let projectContent = compileProjectEvents updatedEvents
    versionDto <-
      traverse
        ( \version -> do
            createdBy <-
              case version.createdBy of
                Just vCreatedBy -> do
                  user <- getUserById vCreatedBy
                  return . Just $ user
                Nothing -> return Nothing
            return $ toVersionList version createdBy
        )
        updatedVersions
    when shouldSave (logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid)
    commentThreadsMap <- catchError (getProjectCommentsByProjectUuid projectUuid Nothing Nothing) (\_ -> return M.empty)
    return $ toContentDTO projectContent commentThreadsMap updatedEvents versionDto
