module Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.Common where

import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newSecretUuid :: U.UUID
newSecretUuid = u' "5d6c1f0a-8e2b-4b3c-9d4e-7f8a9b0c1d2e"

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertKnowledgeModelSecretsInDB :: RequestContext -> [KnowledgeModelSecret] -> WaiSession () ()
assertKnowledgeModelSecretsInDB requestContext expSecrets = do
  secrets <- getOneFromDB findKnowledgeModelSecrets requestContext
  liftIO $ fmap (\secret -> (secret, secret.workspaceUuid)) secrets `shouldBe` fmap (\secret -> (secret, secret.workspaceUuid)) expSecrets
