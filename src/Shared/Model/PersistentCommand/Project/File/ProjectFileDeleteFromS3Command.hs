module Shared.Model.PersistentCommand.Project.File.ProjectFileDeleteFromS3Command where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data ProjectFileDeleteFromS3Command = ProjectFileDeleteFromS3Command
  { projectUuid :: U.UUID
  , fileUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProjectFileDeleteFromS3Command where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectFileDeleteFromS3Command where
  toJSON = genericToJSON jsonOptions
