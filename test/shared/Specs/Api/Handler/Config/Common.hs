module Specs.Api.Handler.Config.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Service.Tenant.Config.ConfigService

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTenantConfigProjectInDB requestContext tcProject = do
  eitherTcProject <- runInContextIO getCurrentTenantConfigProject requestContext
  liftIO $ isRight eitherTcProject `shouldBe` True
  let (Right tcProjectFromDb) = eitherTcProject
  compareDtos tcProjectFromDb tcProject

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
