module Shared.Model.PersistentCommand.User.RemoveUserGroupMembersCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data RemoveUserGroupMembersCommand = RemoveUserGroupMembersCommand
  { userGroupUuid :: U.UUID
  , userUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance FromJSON RemoveUserGroupMembersCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RemoveUserGroupMembersCommand where
  toJSON = genericToJSON jsonOptions
