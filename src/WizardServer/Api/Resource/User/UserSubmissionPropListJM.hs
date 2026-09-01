module WizardServer.Api.Resource.User.UserSubmissionPropListJM where

import Data.Aeson

import Shared.Model.User.UserSubmissionPropList
import Shared.Util.Aeson

instance FromJSON UserSubmissionPropList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionPropList where
  toJSON = genericToJSON jsonOptions
