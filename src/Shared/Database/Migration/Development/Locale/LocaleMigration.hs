module Shared.Database.Migration.Development.Locale.LocaleMigration where

import Data.Foldable (traverse_)

import Shared.Constant.Component
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.S3.Locale.LocaleS3
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Limit/Locale) started"
  deleteLocales
  insertLocale localeDefaultEn
  insertLocale localeNl
  insertLocale localeDe
  insertLocale differentLocale
  logInfo _CMP_MIGRATION "(Limit/Locale) ended"

runS3Migration :: WizardRequestContextC s m => m ()
runS3Migration =
  traverse_
    ( \(uuid, content) -> do
        _ <- putLocale uuid "wizard.json" content
        _ <- putLocale uuid "mail.po" content
        return ()
    )
    [ (localeNl.uuid, localeNlContent)
    , (localeDe.uuid, localeDeContent)
    , (differentLocale.uuid, differentLocaleContent)
    ]
