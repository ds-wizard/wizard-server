module Shared.Integration.Aws.Lambda where

import qualified Amazonka as AWS
import Amazonka.Lambda
import Amazonka.Lambda.Invoke
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Shared.Integration.Aws.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

invokeLambda :: RequestContextC s sc m => String -> BS.ByteString -> m Bool
invokeLambda functionArn payload = do
  let request =
        Invoke'
          { clientContext = Nothing
          , invocationType = Just InvocationType_Event
          , logType = Nothing
          , qualifier = Nothing
          , functionName = T.pack functionArn
          , payload = payload
          }
  logInfoI _CMP_INTEGRATION (show request)
  response <- runAwsRequestWithContext (`AWS.send` request)
  logInfoI _CMP_INTEGRATION (show response)
  return $ response.statusCode == 200
