module Shared.Model.PersistentCommand.Tenant.CreateTenantCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data CreateTenantCommand = CreateTenantCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateTenantCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateTenantCommand where
  toJSON = genericToJSON jsonOptions
