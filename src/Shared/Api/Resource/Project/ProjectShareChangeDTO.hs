module Shared.Api.Resource.Project.ProjectShareChangeDTO where

import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Shared.Model.Project.Project

data ProjectShareChangeDTO = ProjectShareChangeDTO
  { visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , permissions :: [ProjectPermChangeDTO]
  }
  deriving (Show, Eq, Generic)
