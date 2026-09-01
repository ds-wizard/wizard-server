module Shared.Database.Mapping.KnowledgeModel.Cache.KnowledgeModelCache where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.KnowledgeModelCache
import Shared.Util.Uuid

instance ToRow KnowledgeModelCache where
  toRow KnowledgeModelCache {..} =
    [ toField knowledgeModelPackageUuid
    , toField . PGArray $ tagUuids
    , toJSONField knowledgeModel
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelCache where
  fromRow = do
    knowledgeModelPackageUuid <- field
    tagUuidsS <- fromPGArray <$> field
    let tagUuids = fmap u' tagUuidsS
    knowledgeModel <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelCache {..}
