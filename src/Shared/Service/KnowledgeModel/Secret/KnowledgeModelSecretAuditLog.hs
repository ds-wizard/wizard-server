module Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretAuditLog where

import Control.Monad (when)

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Service.AuditLog.AuditLogService

al_createKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecret -> m ()
al_createKnowledgeModelSecret secret = logAuditLogNoData knowledgeModelSecretsCreate (secretEntity secret)

al_updateKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecret -> KnowledgeModelSecret -> m ()
al_updateKnowledgeModelSecret secret updatedSecret =
  when (secret.name /= updatedSecret.name || valueChanged) $
    logAuditLogWithChanges
      knowledgeModelSecretsUpdate
      (secretEntity updatedSecret)
      AuditLogKnowledgeModelSecretChanges {name = change secret.name updatedSecret.name}
      AuditLogKnowledgeModelSecretUpdateData {valueChanged = valueChanged}
  where
    valueChanged = secret.value /= updatedSecret.value

al_deleteKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecret -> m ()
al_deleteKnowledgeModelSecret secret = logAuditLogNoData knowledgeModelSecretsDelete (secretEntity secret)

secretEntity :: KnowledgeModelSecret -> AuditLogEntity
secretEntity secret = AuditLogEntity {uuid = secret.uuid, name = secret.name, workspaceUuid = secret.workspaceUuid}
