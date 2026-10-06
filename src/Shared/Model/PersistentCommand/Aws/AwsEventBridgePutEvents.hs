module Shared.Model.PersistentCommand.Aws.AwsEventBridgePutEvents where

import Data.Aeson
import GHC.Generics

import Shared.Util.Aeson

data AwsEventBridgePutEvents = AwsEventBridgePutEvents
  { action :: String
  , actionData :: Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON AwsEventBridgePutEvents where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AwsEventBridgePutEvents where
  toJSON = genericToJSON jsonOptions
