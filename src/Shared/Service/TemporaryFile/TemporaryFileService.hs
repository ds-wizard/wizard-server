module Shared.Service.TemporaryFile.TemporaryFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Conduit as C
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Network.Minio
import Network.URI.Encode (encode)

import Shared.Database.DAO.Common
import Shared.Database.DAO.TemporaryFile.TemporaryFileDAO
import Shared.Model.Context.RequestContext
import Shared.Model.TemporaryFile.TemporaryFile
import Shared.S3.TemporaryFile.TemporaryFileS3
import Shared.Service.TemporaryFile.TemporaryFileMapper
import Shared.Util.Logger
import Shared.Util.String
import Shared.Util.Uuid

createTemporaryFile :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> BSL.ByteString -> m String
createTemporaryFile fileName contentType mCreatedBy content = do
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCreatedBy now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFile tf.uuid escapedFileName tf.contentType contentDisposition (BSL.toStrict content)
    presignGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

createTemporaryFileConduit :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> Minio (C.ConduitM () BS.ByteString Minio ()) -> m String
createTemporaryFileConduit fileName contentType mCreatedBy contentAction = do
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCreatedBy now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFileConduit tf.uuid escapedFileName tf.contentType contentDisposition contentAction
    presignGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

deleteTemporaryFile :: RequestContextC s sc m => TemporaryFile -> m ()
deleteTemporaryFile tf = do
  deleteTemporaryFileByUuid tf.uuid
  let escapedFileName = encode tf.fileName
  removeTemporaryFile tf.uuid escapedFileName

cleanTemporaryFiles :: RequestContextC s sc m => m ()
cleanTemporaryFiles =
  runInTransaction logInfoI logWarnI $ do
    now <- liftIO getCurrentTime
    tfs <- findTemporaryFilesOlderThen now
    traverse_ deleteTemporaryFile tfs
