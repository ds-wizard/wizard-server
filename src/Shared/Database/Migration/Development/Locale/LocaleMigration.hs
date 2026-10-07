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
  logInfo _CMP_MIGRATION "(Locale/Locale) started"
  deleteLocales
  insertLocale localeDe
  insertLocale localeCs
  insertLocale differentLocale
  logInfo _CMP_MIGRATION "(Locale/Locale) ended"

runS3Migration :: WizardRequestContextC s m => m ()
runS3Migration =
  traverse_
    ( \(locale, po, json) -> do
        _ <- putLocalePo locale.uuid po
        putLocaleJson locale.uuid json
    )
    [ (localeDe, localeDePo, localeDeJson)
    , (localeCs, localeCsPo, localeCsJson)
    , (differentLocale, differentLocalePo, differentLocaleJson)
    ]
