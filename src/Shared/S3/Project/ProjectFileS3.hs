module Shared.S3.Project.ProjectFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.UUID as U
import Network.Minio

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "project-files"

retrieveFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> m BS.ByteString
retrieveFile projectUuid fileUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

retrieveFileConduitAction :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (Minio (C.ConduitM () BS.ByteString Minio ()))
retrieveFileConduitAction projectUuid fileUuid = createGetObjectConduitActionFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

putFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> BS.ByteString -> m String
putFile projectUuid fileUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid]) (Just contentType) Nothing

putFileConduit :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> m String
putFileConduit projectUuid fileUuid contentType contentDisposition = createPutObjectConduitFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid]) (Just contentType) (Just contentDisposition)

presignGetFileUrl :: WizardRequestContextC s m => U.UUID -> U.UUID -> Int -> m String
presignGetFileUrl projectUuid fileUuid = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

removeFiles :: WizardRequestContextC s m => U.UUID -> m ()
removeFiles projectUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString projectUuid])

removeFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeFile projectUuid fileUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

makeBucket :: WizardRequestContextC s m => m ()
makeBucket = createMakeBucketFn

purgeBucket :: WizardRequestContextC s m => m ()
purgeBucket = createPurgeBucketFn

removeBucket :: WizardRequestContextC s m => m ()
removeBucket = createRemoveBucketFn
