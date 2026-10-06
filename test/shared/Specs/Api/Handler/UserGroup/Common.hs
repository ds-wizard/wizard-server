module Specs.Api.Handler.UserGroup.Common where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.User.UserGroup
import Shared.Service.User.Group.UserGroupMapper

import SharedTest.Specs.Api.Common

userGroupChange :: UserGroupChangeDTO
userGroupChange = (toChangeDTO bioGroup) {name = "Edited Group", description = Just "Edited description", private = False} :: UserGroupChangeDTO

workspaceQuery :: BS.ByteString
workspaceQuery = "w=" <> BS.pack (U.toString defaultWorkspaceUuid)

secondWorkspaceQuery :: BS.ByteString
secondWorkspaceQuery = "w=" <> BS.pack (U.toString secondWorkspaceUuid)

compareUserGroups :: UserGroup -> UserGroup -> Bool
compareUserGroups resDto expDto =
  resDto.name == expDto.name
    && resDto.description == expDto.description
    && resDto.private == expDto.private
    && resDto.workspaceUuid == expDto.workspaceUuid

expectJson expStatus expDto response =
  response `shouldRespondWith` ResponseMatcher {matchHeaders = resCtHeader : resCorsHeaders, matchStatus = expStatus, matchBody = bodyEquals (encode expDto)}

expectStatus expStatus response = response `shouldRespondWith` ResponseMatcher {matchHeaders = [], matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
