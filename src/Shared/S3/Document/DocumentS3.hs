module Shared.S3.Document.DocumentS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "documents"

retrieveDocumentContent :: WizardRequestContextC s m => U.UUID -> m BS.ByteString
retrieveDocumentContent documentUuid = createGetObjectFn (f' "%s/%s" [folderName, U.toString documentUuid])

putDocumentContent :: WizardRequestContextC s m => U.UUID -> BS.ByteString -> m String
putDocumentContent documentUuid = createPutObjectFn (f' "%s/%s" [folderName, U.toString documentUuid]) Nothing Nothing

presignGetDocumentUrl :: WizardRequestContextC s m => U.UUID -> Int -> m String
presignGetDocumentUrl documentUuid = createPresignedGetObjectUrl (f' "%s/%s" [folderName, U.toString documentUuid])

removeDocumentContents :: WizardRequestContextC s m => m ()
removeDocumentContents = createRemoveObjectFn folderName

removeDocumentContent :: WizardRequestContextC s m => U.UUID -> m ()
removeDocumentContent documentUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString documentUuid])

removeDocumentContentWithTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeDocumentContentWithTenant tenantUuid documentUuid = createRemoveObjectWithTenantFn tenantUuid (f' "%s/%s" [folderName, U.toString documentUuid])
