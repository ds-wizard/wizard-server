module Shared.Database.Migration.Development.OpenId.OpenIdClientMigration where

import Control.Monad (void)

import Shared.Constant.Component
import Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(OpenId/OpenIdClient) started"
  _ <- deleteOpenIdClientDefinitionDefinitions
  void $ insertOpenIdClientDefinition defaultOpenIdClient
  logInfo _CMP_MIGRATION "(OpenId/OpenIdClient) ended"
