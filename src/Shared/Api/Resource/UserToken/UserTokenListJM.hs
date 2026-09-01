module Shared.Api.Resource.UserToken.UserTokenListJM where

import Data.Aeson

import Shared.Model.User.UserTokenList
import Shared.Util.Aeson

instance FromJSON UserTokenList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenList where
  toJSON = genericToJSON jsonOptions
