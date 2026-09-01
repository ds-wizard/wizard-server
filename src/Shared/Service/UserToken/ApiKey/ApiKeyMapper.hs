module Shared.Service.UserToken.ApiKey.ApiKeyMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Model.User.UserToken
import Shared.Service.UserToken.UserTokenMapper

fromApiKeyDTO :: ApiKeyCreateDTO -> U.UUID -> U.UUID -> String -> Maybe String -> U.UUID -> UTCTime -> String -> UserToken
fromApiKeyDTO reqDto uuid userUuid secret mUserAgent =
  toUserToken uuid reqDto.name ApiKeyUserTokenType userUuid reqDto.expiresAt secret mUserAgent Nothing
