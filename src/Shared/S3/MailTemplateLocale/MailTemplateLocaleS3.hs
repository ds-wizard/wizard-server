module Shared.S3.MailTemplateLocale.MailTemplateLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Constant.Gettext
import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "mail-template-locales"

poFileName = "mail.po"

moFileName = "mail.mo"

defaultPotObject = "mail-templates/mail.pot"

customPotObject = "mail-templates.pot"

retrieveMailTemplateLocalePo :: WizardRequestContextC s m => U.UUID -> m BS.ByteString
retrieveMailTemplateLocalePo localeUuid = createGetObjectFn (mailTemplateLocalePath localeUuid poFileName)

putMailTemplateLocalePo :: WizardRequestContextC s m => U.UUID -> BS.ByteString -> m String
putMailTemplateLocalePo localeUuid = createPutObjectFn (mailTemplateLocalePath localeUuid poFileName) (Just poContentType) Nothing

removeMailTemplateLocaleMo :: WizardRequestContextC s m => U.UUID -> m ()
removeMailTemplateLocaleMo localeUuid = createRemoveObjectFn (mailTemplateLocalePath localeUuid moFileName)

removeMailTemplateLocale :: WizardRequestContextC s m => U.UUID -> m ()
removeMailTemplateLocale localeUuid = createRemoveObjectsWithPrefixFn (f' "%s/%s/" [folderName, U.toString localeUuid])

findDefaultMailTemplatePot :: WizardRequestContextC s m => m (Maybe BS.ByteString)
findDefaultMailTemplatePot = createFindInstanceObjectFn defaultPotObject

findCustomMailTemplatePot :: WizardRequestContextC s m => m (Maybe BS.ByteString)
findCustomMailTemplatePot = createFindObjectFn customPotObject

mailTemplateLocalePath :: U.UUID -> String -> String
mailTemplateLocalePath localeUuid fileName = f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]
