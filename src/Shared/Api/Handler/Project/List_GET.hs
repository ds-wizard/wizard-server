module Shared.Api.Handler.Project.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Common.FromHttpApiData ()
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate
import Shared.Service.Project.ProjectService
import Shared.Util.String (splitOn)

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> QueryParam "q" String
    :> QueryParam "isTemplate" Bool
    :> QueryParam "projectTags" String
    :> QueryParam "projectTagsOp" String
    :> QueryParam "userUuids" [U.UUID]
    :> QueryParam "userUuidsOp" String
    :> QueryParam "userGroupUuids" [U.UUID]
    :> QueryParam "userGroupUuidsOp" String
    :> QueryParam "knowledgeModelPackageIds" [Coordinate]
    :> QueryParam "knowledgeModelPackageIdsOp" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectDTO))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe String
  -> Maybe String
  -> Maybe [U.UUID]
  -> Maybe String
  -> Maybe [U.UUID]
  -> Maybe String
  -> Maybe [Coordinate]
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page ProjectDTO))
list_GET mTokenHeader mServerUrl mQuery mIsTemplate mProjectTagsL mProjectTagsOp mUserUuids mUserUuidsOp mUserGroupUuids mUserGroupUuidsOp mKnowledgeModelPackageCoordinates mKnowledgeModelPackageCoordinatesOp mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mProjectTags = fmap (splitOn ",") mProjectTagsL
        getProjectsForCurrentUserPageDto
          mQuery
          mIsTemplate
          mProjectTags
          mProjectTagsOp
          mUserUuids
          mUserUuidsOp
          mUserGroupUuids
          mUserGroupUuidsOp
          mKnowledgeModelPackageCoordinates
          mKnowledgeModelPackageCoordinatesOp
          (Pageable mPage mSize)
          (parseSortQuery mSort)
