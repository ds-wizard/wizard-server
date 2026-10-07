module Shared.Service.Locale.LocaleAuditLog where

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.LocaleAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Service.AuditLog.AuditLogService

al_createLocale :: WizardRequestContextC s m => Locale -> m ()
al_createLocale locale = logAuditLog localesCreate (localeEntity locale) (toLocaleData locale)

al_replaceLocale :: WizardRequestContextC s m => Locale -> Locale -> m ()
al_replaceLocale locale updatedLocale =
  logAuditLog
    localesReplace
    (localeEntity updatedLocale)
    AuditLogLocaleReplaceData
      { code = updatedLocale.code
      , name = updatedLocale.name
      , oldPotVersion = locale.potVersion
      , newPotVersion = updatedLocale.potVersion
      , officialSwitchedOff = locale.official && not updatedLocale.official
      }

al_updateLocale :: WizardRequestContextC s m => Locale -> Locale -> m ()
al_updateLocale locale updatedLocale =
  logAuditLogChanges
    localesUpdate
    (localeEntity updatedLocale)
    AuditLogLocaleChanges
      { enabled = change locale.enabled updatedLocale.enabled
      , defaultLocale = change locale.defaultLocale updatedLocale.defaultLocale
      }

al_deleteLocale :: WizardRequestContextC s m => Locale -> m ()
al_deleteLocale locale = logAuditLog localesDelete (localeEntity locale) (toLocaleData locale)

al_exportLocale :: WizardRequestContextC s m => Locale -> m ()
al_exportLocale locale = logAuditLog localesExport (localeEntity locale) (toLocaleData locale)

localeEntity :: Locale -> AuditLogEntity
localeEntity locale = entityOf locale.uuid locale.name

toLocaleData :: Locale -> AuditLogLocaleData
toLocaleData locale = AuditLogLocaleData {code = locale.code, name = locale.name, potVersion = locale.potVersion, official = locale.official}
