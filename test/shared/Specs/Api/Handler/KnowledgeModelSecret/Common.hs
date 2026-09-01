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

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareKnowledgeModelSecretDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.value `shouldBe` expDto.value
