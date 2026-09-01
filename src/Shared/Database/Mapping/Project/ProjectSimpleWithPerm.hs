module Shared.Database.Mapping.Project.ProjectSimpleWithPerm where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Database.Mapping.Project.ProjectPerm ()
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Util.String
import Shared.Util.Uuid

instance FromRow ProjectSimpleWithPerm where
  fromRow = do
    uuid <- field
    visibility <- field
    sharing <- field
    tenantUuid <- field
    mUserPermissions <- fieldWith (optionalField fromField)
    let userPermissions =
          case mUserPermissions of
            Just userPermissions -> fmap (parseUserPermission uuid tenantUuid) . fromPGArray $ userPermissions
            Nothing -> []
    mGroupPermissions <- fieldWith (optionalField fromField)
    let groupPermissions =
          case mGroupPermissions of
            Just groupPermissions -> fmap (parseGroupPermission uuid tenantUuid) . fromPGArray $ groupPermissions
            Nothing -> []
    let permissions = userPermissions ++ groupPermissions
    return $ ProjectSimpleWithPerm {..}
    where
      parseUserPermission :: U.UUID -> U.UUID -> String -> ProjectPerm
      parseUserPermission projectUuid tenantUuid permission =
        let parts = splitOn "::" permission
         in ProjectPerm
              { projectUuid = projectUuid
              , memberType = UserProjectPermType
              , memberUuid = u' $ head parts
              , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , tenantUuid = tenantUuid
              }
      parseGroupPermission :: U.UUID -> U.UUID -> String -> ProjectPerm
      parseGroupPermission projectUuid tenantUuid permission =
        let parts = splitOn "::" permission
         in ProjectPerm
              { projectUuid = projectUuid
              , memberType = UserGroupProjectPermType
              , memberUuid = u' $ head parts
              , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , tenantUuid = tenantUuid
              }
