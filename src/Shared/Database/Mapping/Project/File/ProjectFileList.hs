module Shared.Database.Mapping.Project.File.ProjectFileList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.Project.ProjectSimple
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.User.UserSuggestion
import Shared.Util.Gravatar

instance FromRow ProjectFileList where
  fromRow = do
    uuid <- field
    fileName <- field
    contentType <- field
    fileSize <- field
    createdAt <- field
    project <- fieldProjectSimple
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
    return $ ProjectFileList {..}
