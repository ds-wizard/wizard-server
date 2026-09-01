module Shared.Model.Document.DocumentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate

data DocumentList = DocumentList
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , projectUuid :: U.UUID
  , projectName :: String
  , projectEventUuid :: Maybe U.UUID
  , projectVersion :: Maybe String
  , documentTemplate :: DocumentTemplateWithCoordinate
  , documentTemplateFormat :: DocumentTemplateFormatSimple
  , language :: Maybe String
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
