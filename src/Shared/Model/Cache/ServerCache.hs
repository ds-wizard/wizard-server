module Shared.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Shared.Model.User.UserToken

data ServerCache = ServerCache
  { userToken :: C.Cache Int UserToken
  }
