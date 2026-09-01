module Shared.Database.Mapping.Project.Version.ProjectVersionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.User.UserSuggestion
import Shared.Util.Gravatar

instance FromRow ProjectVersionList where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    eventUuid <- field
    createdAt <- field
    updatedAt <- field
    createdByUuid <- field
    createdByFirstName <- field
    createdByLastName <- field
    createdByEmail <- field
    createdByImageUrl <- field
    createdByAffiliation <- field
    let createdBy =
          case (createdByUuid, createdByFirstName, createdByLastName, createdByEmail, createdByImageUrl, createdByAffiliation) of
            (Just uuid, Just firstName, Just lastName, Just email, imageUrl, affiliation) ->
              let gravatarHash = createGravatarHash email
               in Just UserSuggestion {..}
            _ -> Nothing
    return $ ProjectVersionList {..}
