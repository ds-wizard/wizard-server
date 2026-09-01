module Shared.Service.Audit.AuditService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Audit.AuditDAO
import Shared.Model.Context.RequestContext
import Shared.Service.Audit.AuditMapper
import Shared.Util.Uuid

logAudit :: RequestContextC s sc m => String -> String -> String -> m ()
logAudit component action entity = logAuditWithBody component action entity M.empty

logAuditWithBody :: RequestContextC s sc m => String -> String -> String -> M.Map String String -> m ()
logAuditWithBody component action entity body = do
  aUuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  createdBy <- asks (.identity')
  now <- liftIO getCurrentTime
  let audit = toAudit aUuid component action entity body (fmap (fromJust . U.fromString) createdBy) tenantUuid now
  insertAudit audit
  return ()
