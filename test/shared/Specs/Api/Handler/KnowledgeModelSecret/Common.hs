module Specs.Api.Handler.KnowledgeModelSecret.Common where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Model.KnowledgeModel.KnowledgeModelSecret

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfKnowledgeModelSecretInDB requestContext kmSecret = do
  kmSecretFromDb <- getFirstFromDB findKnowledgeModelSecrets requestContext
  compareKnowledgeModelSecretDtos kmSecretFromDb kmSecret
  liftIO $ kmSecretFromDb.value `shouldBe` kmSecret.value

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareKnowledgeModelSecretDtos resDto expDto = liftIO $ resDto.name `shouldBe` expDto.name
