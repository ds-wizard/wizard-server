module Shared.Model.Project.ProjectSimpleWithPerm where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project

data ProjectSimpleWithPerm = ProjectSimpleWithPerm
  { uuid :: U.UUID
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , tenantUuid :: U.UUID
  , permissions :: [ProjectPerm]
  }
  deriving (Generic, Eq, Show)
