module Shared.Database.Mapping.Project.ProjectDetailQuestionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Database.Mapping.Project.File.ProjectFileSimple ()
import Shared.Database.Mapping.Project.ProjectAcl
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectState ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Project.Detail.ProjectDetailQuestionnaire
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Util.String
import Shared.Util.Uuid

instance FromRow ProjectDetailQuestionnaire where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackage <- fieldKnowledgeModelPackageSuggestion
    selectedQuestionTagUuids <- fromPGArray <$> field
    language <- field
    isTemplate <- field
    permissions <- loadPermissions uuid
    mFiles <- fieldWith (optionalField fromField)
    let files =
          case mFiles of
            Just files -> fmap parseFile . fromPGArray $ files
            Nothing -> []
    return $ ProjectDetailQuestionnaire {..}
    where
      parseFile :: String -> ProjectFileSimple
      parseFile file =
        let parts = splitOn "<:::::>" file
         in ProjectFileSimple
              { uuid = u' $ head parts
              , fileName = parts !! 1
              , contentType = parts !! 2
              , fileSize = read $ parts !! 3
              }
