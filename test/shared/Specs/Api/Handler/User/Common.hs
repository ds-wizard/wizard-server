module Specs.Api.Handler.User.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserSubmissionPropDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.User.UserSubmissionProp
import Shared.Model.User.UserToken
import Shared.Service.User.UserUtil
import WizardServer.Model.Context.RequestContext

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserInDB requestContext user = do
  eUser <- runInContextIO (findUserByUuid user.uuid) requestContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  compareUserDtos userFromDB user

assertPasswordOfUserInDB requestContext user password = do
  eUser <- runInContextIO (findUserByUuid user.uuid) requestContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  let isSame = verifyPassword password userFromDB.passwordHash
  liftIO $ isSame `shouldBe` True

assertAbsenceOfUserInDB requestContext user = do
  eUser <- runInContextIO (findUserByUuid user.uuid) requestContext
  liftIO $ isLeft eUser `shouldBe` True
  let (Left error) = eUser
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "user_entity" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString user.uuid)])

assertExistenceOfUserSubmissionPropsInDB :: RequestContext -> User -> [UserSubmissionProp] -> WaiSession st ()
assertExistenceOfUserSubmissionPropsInDB requestContext user submissionProps = do
  eSubmissionProps <- runInContextIO (findUserSubmissionProps user.uuid) requestContext
  liftIO $ isRight eSubmissionProps `shouldBe` True
  let (Right submissionPropsFromDB) = eSubmissionProps
  liftIO $ (submissionPropsFromDB == submissionProps) `shouldBe` True

assertUserTokenInDB requestContext user size = do
  eUserTokens <- runInContextIO (findUserTokensByUserUuid user.uuid) requestContext
  liftIO $ isRight eUserTokens `shouldBe` True
  let (Right userTokens) = eUserTokens
  liftIO $ length userTokens `shouldBe` size

assertExistenceOfUserTokenInDB requestContext user token = do
  eUserTokens <- runInContextIO (findUserTokensByUserUuid user.uuid) requestContext
  liftIO $ isRight eUserTokens `shouldBe` True
  let (Right [userToken]) = eUserTokens
  liftIO $ userToken.value `shouldBe` token

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareUserDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareUserCreateDtos resDto expDto userActive = do
  liftIO $ resDto.firstName `shouldBe` expDto.firstName
  liftIO $ resDto.lastName `shouldBe` expDto.lastName
  liftIO $ resDto.email `shouldBe` expDto.email
  liftIO $ resDto.affiliation `shouldBe` expDto.affiliation
  liftIO $ Just resDto.role.uuid `shouldBe` expDto.roleUuid
  liftIO $ resDto.active `shouldBe` userActive
