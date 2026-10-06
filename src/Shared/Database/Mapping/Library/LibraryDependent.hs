module Shared.Database.Mapping.Library.LibraryDependent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.Common
import Shared.Model.Library.LibraryDependents

instance FromField LibraryDependentEntity where
  fromField = fromFieldGenericEnum

instance FromRow LibraryDependent where
  fromRow = do
    entity <- field
    uuid <- field
    name <- field
    pId <- field
    version <- field
    workspaceUuid <- field
    visible <- field
    return $ LibraryDependent {..}
