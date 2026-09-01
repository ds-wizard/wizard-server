module Shared.Service.Project.Event.ProjectEventService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Lens
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Project.ProjectAcl
import Shared.Service.User.WizardUserMapper
import Shared.Util.List (dropWhileExclusive, groupBy, takeWhileInclusive)
import Shared.Util.Logger

addEventToProject :: WizardRequestContextC s m => U.UUID -> ProjectEventChangeDTO -> m ()
addEventToProject projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkEditPermissionToProject project.visibility project.sharing project.permissions
    mCurrentUser <- asks (.currentUser')
    let mCreatedBy = fmap toSuggestion' mCurrentUser
    addEvent projectUuid EditorWebsocketPerm mCreatedBy reqDto

squashProjectEvents :: WizardRequestContextC s m => m ()
squashProjectEvents = do
  projectUuids <- findProjectForSquashing
  traverse_ squashProjectEventsForProject projectUuids

squashProjectEventsForProject :: WizardRequestContextC s m => U.UUID -> m ()
squashProjectEventsForProject projectUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for project (projectUuid: '%s')" [U.toString projectUuid])
    events <- findProjectEventsByProjectUuid projectUuid
    versions <- findProjectVersionsByProjectUuid projectUuid
    let squashedEvents = squash versions events
    syncProjectEventsWithDb events squashedEvents
    updateProjectSquashedByUuid projectUuid True
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for project '%s' finished successfully (before: %s, after %s)"
          [U.toString projectUuid, show . length $ events, show . length $ squashedEvents]
      )

instance Ord ProjectEvent where
  compare a b = compare (getCreatedAt a) (getCreatedAt b)

squash :: [ProjectVersion] -> [ProjectEvent] -> [ProjectEvent]
squash versions events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (getCreatedAt e1) == utctDay (getCreatedAt e2)) events
      squashedEvents = fmap squashOnePeriod (concatMap (splitAfterVersion versions) groupedEvents)
   in concat squashedEvents

splitAfterVersion :: [ProjectVersion] -> [ProjectEvent] -> [[ProjectEvent]]
splitAfterVersion _ [] = []
splitAfterVersion versions events =
  let isNotVersion event = not . L.any (\v -> v.eventUuid == getUuid event) $ versions
   in takeWhileInclusive isNotVersion events : splitAfterVersion versions (dropWhileExclusive isNotVersion events)

squashOnePeriod :: [ProjectEvent] -> [ProjectEvent]
squashOnePeriod = snd . foldr go (M.empty, [])
  where
    go
      :: ProjectEvent
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
    go event' (questions, events) =
      case event' of
        SetReplyEvent' event ->
          if Just event.createdBy == M.lookup event.path questions
            then (questions, events)
            else (M.insert event.path event.createdBy questions, event' : events)
        _ -> (questions, event' : events)
