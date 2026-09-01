module Shared.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "locales"

retrieveLocale :: WizardRequestContextC s m => U.UUID -> String -> m BS.ByteString
retrieveLocale localeUuid filename = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, filename])

retrieveLocaleWithTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> m BS.ByteString
retrieveLocaleWithTenant tenantUuid localeUuid fileName = createGetObjectWithTenantFn tenantUuid (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName])

putLocale :: WizardRequestContextC s m => U.UUID -> String -> BS.ByteString -> m String
putLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeLocales :: WizardRequestContextC s m => m ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: WizardRequestContextC s m => U.UUID -> m ()
removeLocale localeUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString localeUuid])
