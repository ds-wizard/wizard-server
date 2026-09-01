module Shared.Model.Project.File.ProjectFileList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Model.Project.ProjectSimple
import Shared.Model.User.UserSuggestion

data ProjectFileList = ProjectFileList
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , project :: ProjectSimple
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
