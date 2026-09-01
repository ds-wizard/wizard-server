module Shared.Database.Migration.Development.User.Data.WizardUserTokens (
  module Shared.Database.Migration.Development.User.Data.WizardUserTokens,
  module Shared.Database.Migration.Development.User.Data.UserTokens,
) where

import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Database.Migration.Development.User.Data.UserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Service.UserToken.UserTokenMapper
import Shared.Util.Date

albertTokenDto :: UserTokenDTO
albertTokenDto = toDTO albertToken

albertCreateToken :: LoginDTO
albertCreateToken =
  LoginDTO {email = userAlbert.email, password = "password", code = Nothing}

albertCreateApiKey :: ApiKeyCreateDTO
albertCreateApiKey =
  ApiKeyCreateDTO {name = albertApiKey.name, expiresAt = dt' 2052 1 21}

nikolaCreateToken :: LoginDTO
nikolaCreateToken =
  LoginDTO {email = userNikola.email, password = "password", code = Nothing}

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
