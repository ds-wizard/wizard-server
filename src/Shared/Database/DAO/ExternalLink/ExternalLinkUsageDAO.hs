module Shared.Database.DAO.ExternalLink.ExternalLinkUsageDAO where

import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.ExternalLink.ExternalLinkUsage ()
import Shared.Model.Context.RequestContext
import Shared.Model.ExternalLink.ExternalLinkUsage

entityName = "external_link_usage"

findExternalLinkUsages :: RequestContextC s sc m => m [ExternalLinkUsage]
findExternalLinkUsages = createFindEntitiesFn entityName

insertExternalLinkUsage :: RequestContextC s sc m => ExternalLinkUsage -> m Int64
insertExternalLinkUsage = createInsertFn entityName

deleteExternalLinkUsages :: RequestContextC s sc m => m Int64
deleteExternalLinkUsages = createDeleteEntitiesFn entityName
