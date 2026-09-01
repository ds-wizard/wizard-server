module Specs.Api.Handler.Document.Common where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.Document.Document
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentInDB requestContext reqDto = do
  docFromDb <- getFirstFromDB findDocuments requestContext
  liftIO $ docFromDb.name `shouldBe` reqDto.name
  liftIO $ docFromDb.projectUuid `shouldBe` Just reqDto.projectUuid
  liftIO $ docFromDb.documentTemplateUuid `shouldBe` reqDto.documentTemplateUuid
  liftIO $ docFromDb.formatUuid `shouldBe` reqDto.formatUuid

assertAbsenceOfDocumentInDB requestContext doc = do
  eDoc <- runInContextIO (findDocumentByUuid doc.uuid) requestContext
  liftIO $ isLeft eDoc `shouldBe` True
  let (Left error) = eDoc
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "document" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString doc.uuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ (fromJust resDto.project).uuid `shouldBe` expDto.projectUuid
  liftIO $ resDto.format.uuid `shouldBe` expDto.formatUuid
