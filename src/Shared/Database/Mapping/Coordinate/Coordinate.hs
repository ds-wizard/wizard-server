module Shared.Database.Mapping.Coordinate.Coordinate where

import Database.PostgreSQL.Simple.FromRow

import Shared.Model.Coordinate.Coordinate

coordinateFromFields :: RowParser (Maybe Coordinate)
coordinateFromFields = do
  mId <- field
  mVersion <- field
  return $ Coordinate <$> mId <*> mVersion
