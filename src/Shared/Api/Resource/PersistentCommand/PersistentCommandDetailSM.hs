module Shared.Api.Resource.PersistentCommand.PersistentCommandDetailSM where

import Data.Swagger

import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Shared.Api.Resource.PersistentCommand.WizardPersistentCommandSM ()
import Shared.Api.Resource.Tenant.WizardTenantSM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Service.PersistentCommand.WizardPersistentCommandMapper
import qualified Shared.Service.Tenant.TenantMapper as TNT_Mapper
import Shared.Util.Swagger

instance ToSchema PersistentCommandDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO command1 (Just userAlbert) (TNT_Mapper.toDTO defaultTenant Nothing Nothing))
