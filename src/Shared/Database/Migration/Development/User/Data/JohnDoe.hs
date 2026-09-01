module Shared.Database.Migration.Development.User.Data.JohnDoe where

import Shared.Api.Resource.User.UserCreateDTO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.User.Role

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
    { firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@example.com"
    , affiliation = Just "My University"
    , roleUuid = Just adminRole.uuid
    , password = "password"
    }

userJohnCreateDS :: UserCreateDTO
userJohnCreateDS = userJohnCreate {roleUuid = Just researcherRole.uuid}
