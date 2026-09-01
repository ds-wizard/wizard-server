module Shared.Model.User.UserRegistrationPendingServiceType where

import GHC.Generics

data UserRegistrationPendingServiceType
  = OpenIdUserRegistrationPendingServiceType
  deriving (Show, Eq, Generic, Read)
