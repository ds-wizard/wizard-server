module Shared.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Database.Mapping.Coordinate.Coordinate
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

instance FromRow KnowledgeModelBundlePackage where
  fromRow = do
    id <- field
    name <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageId <- coordinateFromFields
    forkOfPackageId <- coordinateFromFields
    mergeCheckpointPackageId <- coordinateFromFields
    events <- fieldWith fromJSONField
    nonEditable <- field
    createdAt <- field
    language <- field
    return $ KnowledgeModelBundlePackage {..}
