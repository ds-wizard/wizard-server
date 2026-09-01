module Shared.Api.Resource.UserEmailLink.UserEmailLinkTypeJM where

import Data.Aeson

import Shared.Model.UserEmailLink.UserEmailLinkType

instance FromJSON UserEmailLinkType

instance ToJSON UserEmailLinkType
