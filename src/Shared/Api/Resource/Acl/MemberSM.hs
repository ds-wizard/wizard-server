module Shared.Api.Resource.Acl.MemberSM where

import Data.Swagger

import Shared.Api.Resource.Acl.MemberDTO
import Shared.Api.Resource.Acl.MemberJM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Service.Acl.AclMapper
import Shared.Util.Swagger

instance ToSchema MemberDTO where
  declareNamedSchema = toSwagger (toUserMemberDTO userAlbert)
