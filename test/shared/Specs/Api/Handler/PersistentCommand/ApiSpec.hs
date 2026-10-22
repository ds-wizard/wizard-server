module Specs.Api.Handler.PersistentCommand.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.PersistentCommand.Detail_Attempts_POST

persistentCommandAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "PERSISTENT COMMAND API Spec" $
      detail_attempts_POST requestContext
