module Shared.Api.Handler.Project.File.Detail_Download_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.File.ProjectFileService

type Detail_Download_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_download_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_download_GET mTokenHeader mServerUrl projectUuid fileUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< downloadProjectFile projectUuid fileUuid
