module Shared.Service.Project.File.ProjectFileMapper where

import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.File.FileCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import qualified Shared.Service.Project.ProjectMapper as ProjectMapper
import qualified Shared.Service.User.WizardUserMapper as UserMapper

toList :: ProjectFile -> Project -> Maybe UserDTO -> ProjectFileList
toList ProjectFile {..} project mCreatedBy =
  ProjectFileList
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , project = ProjectMapper.toSimple project
    , createdBy = fmap UserMapper.toSuggestion' mCreatedBy
    , createdAt = createdAt
    }

toSimple :: ProjectFile -> ProjectFileSimple
toSimple ProjectFile {..} =
  ProjectFileSimple
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    }

fromFileCreateDTO :: FileCreateDTO -> U.UUID -> U.UUID -> Maybe UserDTO -> U.UUID -> UTCTime -> ProjectFile
fromFileCreateDTO reqDto uuid projectUuid mCreatedBy tenantUuid now =
  ProjectFile
    { uuid = uuid
    , fileName = reqDto.fileName
    , contentType = reqDto.contentType
    , fileSize = fromIntegral . BS.length $ reqDto.content
    , projectUuid = projectUuid
    , createdBy = fmap (.uuid) mCreatedBy
    , tenantUuid = tenantUuid
    , createdAt = now
    }
