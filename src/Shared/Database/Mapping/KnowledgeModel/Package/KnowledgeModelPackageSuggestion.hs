module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

instance FromRow KnowledgeModelPackageSuggestion where
  fromRow = do
    uuid <- field
    name <- field
    id <- field
    version <- field
    description <- field
    return $ KnowledgeModelPackageSuggestion {..}

fieldKnowledgeModelPackageSuggestion :: RowParser KnowledgeModelPackageSuggestion
fieldKnowledgeModelPackageSuggestion = do
  uuid <- field
  name <- field
  id <- field
  version <- field
  description <- field
  return KnowledgeModelPackageSuggestion {..}

fieldKnowledgeModelPackageSuggestion' :: RowParser (Maybe KnowledgeModelPackageSuggestion)
fieldKnowledgeModelPackageSuggestion' = do
  mUuid <- field
  mName <- field
  mId <- field
  mVersion <- field
  mDescription <- field
  case (mUuid, mName, mId, mVersion, mDescription) of
    (Just uuid, Just name, Just id, Just version, Just description) -> return $ Just KnowledgeModelPackageSuggestion {..}
    _ -> return Nothing
