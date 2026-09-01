module Shared.Api.Resource.UserEmailLink.UserEmailLinkTypeSM where

import Data.Swagger

import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.Api.Resource.UserEmailLink.UserEmailLinkTypeJM ()
import Shared.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Util.Swagger

instance ToSchema UserEmailLinkType

instance ToSchema (UserEmailLinkDTO UserEmailLinkType) where
  declareNamedSchema = toSwagger forgottenPasswordUserEmailLinkDto
