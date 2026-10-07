module Shared.Service.MailTemplateLocale.MailTemplateLocaleAuditLog where

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.LocaleAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Service.AuditLog.AuditLogService

al_createMailTemplateLocale :: WizardRequestContextC s m => MailTemplateLocale -> m ()
al_createMailTemplateLocale locale = logAuditLog mailTemplateLocalesCreate (localeEntity locale) (toLocaleData locale)

al_replaceMailTemplateLocale :: WizardRequestContextC s m => MailTemplateLocale -> MailTemplateLocale -> m ()
al_replaceMailTemplateLocale locale updatedLocale =
  logAuditLog
    mailTemplateLocalesReplace
    (localeEntity updatedLocale)
    AuditLogLocaleReplaceData
      { code = updatedLocale.code
      , name = updatedLocale.name
      , oldPotVersion = locale.potVersion
      , newPotVersion = updatedLocale.potVersion
      , officialSwitchedOff = locale.official && not updatedLocale.official
      }

al_updateMailTemplateLocale :: WizardRequestContextC s m => MailTemplateLocale -> MailTemplateLocale -> m ()
al_updateMailTemplateLocale locale updatedLocale =
  logAuditLogChanges
    mailTemplateLocalesUpdate
    (localeEntity updatedLocale)
    AuditLogLocaleChanges
      { enabled = change locale.enabled updatedLocale.enabled
      , defaultLocale = change locale.defaultLocale updatedLocale.defaultLocale
      }

al_deleteMailTemplateLocale :: WizardRequestContextC s m => MailTemplateLocale -> m ()
al_deleteMailTemplateLocale locale = logAuditLog mailTemplateLocalesDelete (localeEntity locale) (toLocaleData locale)

al_exportMailTemplateLocale :: WizardRequestContextC s m => MailTemplateLocale -> m ()
al_exportMailTemplateLocale locale = logAuditLog mailTemplateLocalesExport (localeEntity locale) (toLocaleData locale)

localeEntity :: MailTemplateLocale -> AuditLogEntity
localeEntity locale = entityOf locale.uuid locale.name

toLocaleData :: MailTemplateLocale -> AuditLogLocaleData
toLocaleData locale = AuditLogLocaleData {code = locale.code, name = locale.name, potVersion = locale.potVersion, official = locale.official}
