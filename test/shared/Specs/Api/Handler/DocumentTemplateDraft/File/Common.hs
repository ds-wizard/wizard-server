module Specs.Api.Handler.DocumentTemplateDraft.File.Common where

import Data.Either (isLeft)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateFileInDB requestContext file = do
  fileFromDB <- getOneFromDB (findFileByUuid file.uuid) requestContext
  compareTemplateFileDtos fileFromDB file

assertAbsenceOfTemplateFileInDB requestContext file = do
  eFile <- runInContextIO (findFileByUuid file.uuid) requestContext
  liftIO $ isLeft eFile `shouldBe` True
  let (Left error) = eFile
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "document_template_file"
            [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString file.uuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateFileDtos resDto expDto = do
  liftIO $ resDto.fileName `shouldBe` expDto.fileName
  liftIO $ resDto.content `shouldBe` expDto.content
