module Shared.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Constant.Gettext
import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "locales"

poFileName = "ui.po"

jsonFileName = "ui.json"

retrieveLocalePo :: WizardRequestContextC s m => U.UUID -> m BS.ByteString
retrieveLocalePo localeUuid = createGetObjectFn (localePath localeUuid poFileName)

retrieveLocaleJsonWithTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> m BS.ByteString
retrieveLocaleJsonWithTenant tenantUuid localeUuid = createGetObjectWithTenantFn tenantUuid (localePath localeUuid jsonFileName)

putLocalePo :: WizardRequestContextC s m => U.UUID -> BS.ByteString -> m String
putLocalePo localeUuid = createPutObjectFn (localePath localeUuid poFileName) (Just poContentType) Nothing

putLocaleJson :: WizardRequestContextC s m => U.UUID -> BS.ByteString -> m String
putLocaleJson localeUuid = createPutObjectFn (localePath localeUuid jsonFileName) (Just "application/json") Nothing

removeLocale :: WizardRequestContextC s m => U.UUID -> m ()
removeLocale localeUuid = createRemoveObjectsWithPrefixFn (f' "%s/%s/" [folderName, U.toString localeUuid])

localePath :: U.UUID -> String -> String
localePath localeUuid fileName = f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]
