module Shared.Database.DAO.Audit.AuditDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Audit.Audit ()
import Shared.Model.Audit.Audit
import Shared.Model.Context.RequestContext

entityName = "audit"

findAudits :: RequestContextC s sc m => m [Audit]
findAudits = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

insertAudit :: RequestContextC s sc m => Audit -> m Int64
insertAudit = createInsertWithoutTransactionFn entityName

deleteAudits :: RequestContextC s sc m => m Int64
deleteAudits = createDeleteEntitiesFn entityName
