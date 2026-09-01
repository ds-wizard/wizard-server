module Shared.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "document-templates"

potFileName :: String -> String -> String -> String
potFileName organizationId templateId version = f' "%s_%s_%s.pot" [organizationId, templateId, version]

retrievePotFile :: WizardRequestContextC s m => U.UUID -> String -> m BS.ByteString
retrievePotFile documentTemplateUuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, fileName])

putPotFile :: WizardRequestContextC s m => U.UUID -> String -> BS.ByteString -> m String
putPotFile documentTemplateUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, fileName]) Nothing Nothing

retrieveAsset :: WizardRequestContextC s m => U.UUID -> U.UUID -> m BS.ByteString
retrieveAsset documentTemplateUuid assetUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

putAsset :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> BS.ByteString -> m String
putAsset documentTemplateUuid assetUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid]) (Just contentType) Nothing

presignGetAssetUrl :: WizardRequestContextC s m => U.UUID -> U.UUID -> Int -> m String
presignGetAssetUrl documentTemplateUuid assetUuid = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

removeAssets :: WizardRequestContextC s m => U.UUID -> m ()
removeAssets documentTemplateUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString documentTemplateUuid])

removeAsset :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeAsset documentTemplateUuid assetUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

makeBucket :: WizardRequestContextC s m => m ()
makeBucket = createMakeBucketFn

purgeBucket :: WizardRequestContextC s m => m ()
purgeBucket = createPurgeBucketFn

removeBucket :: WizardRequestContextC s m => m ()
removeBucket = createRemoveBucketFn
