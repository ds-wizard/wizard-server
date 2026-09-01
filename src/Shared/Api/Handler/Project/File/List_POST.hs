module Shared.Api.Handler.Project.File.List_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.File.FileCreateDTO
import Shared.Api.Resource.File.FileCreateJM ()
import Shared.Api.Resource.Project.File.ProjectFileListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.File.ProjectFileList
import Shared.Service.Project.File.ProjectFileService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileCreateDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "files"
    :> Capture "questionUuid" U.UUID
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectFileList)

list_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> FileCreateDTO -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] ProjectFileList)
list_POST mTokenHeader mServerUrl reqDto projectUuid questionUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        createProjectFile projectUuid questionUuid reqDto
