module Specs.Api.Handler.DocumentTemplateDraft.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentTemplateInDB requestContext dt = do
  eDt <- runInContextIO (findDraftByUuid dt.uuid) requestContext
  liftIO $ isRight eDt `shouldBe` True
  let (Right dtFromDB) = eDt
  compareDtos dtFromDB dt

assertExistenceOfDraftDataInDB requestContext draftData = do
  eDraftData <- runInContextIO (findDraftDataByUuid draftData.documentTemplateUuid) requestContext
  liftIO $ isRight eDraftData `shouldBe` True
  let (Right draftDataFromDB) = eDraftData
  liftIO $ draftDataFromDB `shouldBe` draftData

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.templateId `shouldBe` expDto.templateId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
