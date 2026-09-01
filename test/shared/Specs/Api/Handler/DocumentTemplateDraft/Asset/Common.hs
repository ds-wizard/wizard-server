module Specs.Api.Handler.DocumentTemplateDraft.Asset.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateAssetInDB requestContext asset = do
  eTemplateAsset <- runInContextIO (findAssetById asset.uuid) requestContext
  liftIO $ isRight eTemplateAsset `shouldBe` True
  let (Right templateAssetFromDB) = eTemplateAsset
  compareDtos templateAssetFromDB asset

assertAbsenceOfTemplateAssetInDB requestContext asset = do
  eAsset <- runInContextIO (findAssetById asset.uuid) requestContext
  liftIO $ isLeft eAsset `shouldBe` True
  let (Left error) = eAsset
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "document_template_asset"
            [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString asset.uuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.fileName `shouldBe` expDto.fileName
  liftIO $ resDto.contentType `shouldBe` expDto.contentType
  liftIO $ resDto.fileSize `shouldBe` expDto.fileSize
