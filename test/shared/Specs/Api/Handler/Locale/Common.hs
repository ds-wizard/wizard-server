module Specs.Api.Handler.Locale.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Locale.Locale

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfLocaleInDB requestContext locale = do
  eLocale <- runInContextIO (findLocaleByUuid locale.uuid) requestContext
  liftIO $ isRight eLocale `shouldBe` True
  let (Right localeFromDB) = eLocale
  compareLocaleDtos localeFromDB locale

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareLocaleDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
  liftIO $ resDto.defaultLocale `shouldBe` expDto.defaultLocale
