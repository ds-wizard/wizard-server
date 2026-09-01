module Shared.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Model.Project.ProjectSimple
import Shared.Model.Submission.SubmissionList

data DocumentDTO = DocumentDTO
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , project :: Maybe ProjectSimple
  , projectEventUuid :: Maybe U.UUID
  , projectVersion :: Maybe String
  , documentTemplate :: DocumentTemplateWithCoordinate
  , format :: DocumentTemplateFormatSimple
  , language :: Maybe String
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , submissions :: [SubmissionList]
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
