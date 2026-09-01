module Specs.Api.Handler.KnowledgeModelEditor.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfEditorInDB requestContext kmEditor previousPackageUuid forkOfPackageId createdBy = do
  editorFromDb <- getFirstFromDB findKnowledgeModelEditors requestContext
  compareKnowledgeModelEditor editorFromDb kmEditor previousPackageUuid forkOfPackageId createdBy

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareKnowledgeModelEditor resDto expDto previousPackageUuid forkOfPackageId createdBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.previousPackageUuid `shouldBe` previousPackageUuid
  liftIO $ resDto.createdBy `shouldBe` createdBy

compareEditorDtos resDto expDto previousPackage forkOfPackageId createdBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.readme `shouldBe` expDto.readme
  liftIO $ resDto.license `shouldBe` expDto.license
  liftIO $ resDto.previousPackage `shouldBe` previousPackage
  liftIO $ resDto.createdBy `shouldBe` createdBy
