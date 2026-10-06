module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToRow KnowledgeModelPackage where
  toRow KnowledgeModelPackage {..} =
    [ toField uuid
    , toField name
    , toField id
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toField previousPackageUuid
    , toField (fmap (.id) forkOfPackageId)
    , toField (fmap (.id) mergeCheckpointPackageId)
    , toField createdAt
    , toField tenantUuid
    , toField phase
    , toField nonEditable
    , toField public
    , toField language
    , toField workspaceUuid
    , toField (fmap (.version) forkOfPackageId)
    , toField (fmap (.version) mergeCheckpointPackageId)
    ]

instance FromRow KnowledgeModelPackage where
  fromRow = do
    uuid <- field
    name <- field
    id <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageUuid <- field
    mForkOfPackageId <- field
    mMergeCheckpointPackageId <- field
    createdAt <- field
    tenantUuid <- field
    phase <- field
    nonEditable <- field
    public <- field
    language <- field
    workspaceUuid <- field
    mForkOfPackageVersion <- field
    mMergeCheckpointPackageVersion <- field
    let forkOfPackageId = Coordinate <$> mForkOfPackageId <*> mForkOfPackageVersion
    let mergeCheckpointPackageId = Coordinate <$> mMergeCheckpointPackageId <*> mMergeCheckpointPackageVersion
    return $ KnowledgeModelPackage {..}
