module Shared.Api.Resource.Project.Acl.ProjectPermSM where

import Data.Swagger

import Shared.Api.Resource.Acl.MemberSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectPermType

instance ToSchema ProjectPerm where
  declareNamedSchema = toSwagger bioGroupEditProjectPerm

instance ToSchema ProjectPermDTO where
  declareNamedSchema =
    toSwagger (toUserProjectPermDTO bioGroupEditProjectPerm userAlbert)
