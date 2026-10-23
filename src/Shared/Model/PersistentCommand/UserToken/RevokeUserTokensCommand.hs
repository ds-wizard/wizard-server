module Shared.Model.PersistentCommand.UserToken.RevokeUserTokensCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data RevokeUserTokensCommand = RevokeUserTokensCommand
  { userUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON RevokeUserTokensCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RevokeUserTokensCommand where
  toJSON = genericToJSON jsonOptions
