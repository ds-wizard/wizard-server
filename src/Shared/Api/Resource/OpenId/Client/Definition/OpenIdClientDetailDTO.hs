module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.OpenId.OpenIdClientParameter
import Shared.Model.OpenId.OpenIdClientStyle

data OpenIdClientDetailDTO = OpenIdClientDetailDTO
  { uuid :: U.UUID
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameters :: [OpenIdClientParameter]
  , style :: OpenIdClientStyle
  , registrationEnabled :: Bool
  , scopeProfile :: Bool
  , scopeEmail :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)
