module Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets where

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Tenant.Tenant
import Shared.Model.Workspace.Workspace
import Shared.Util.Date
import Shared.Util.Uuid

kmSecret1 :: KnowledgeModelSecret
kmSecret1 =
  KnowledgeModelSecret
    { uuid = u' "171635b5-d5e7-4bba-8dd0-93765866aea1"
    , name = "mySecret1"
    , value = "mySecretValue1"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

kmSecret1Edited :: KnowledgeModelSecret
kmSecret1Edited =
  KnowledgeModelSecret
    { uuid = u' "171635b5-d5e7-4bba-8dd0-93765866aea1"
    , name = "EDITED_mySecret1"
    , value = "EDITED_mySecretValue1"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

kmSecret1ChangeDTO :: KnowledgeModelSecretChangeDTO
kmSecret1ChangeDTO =
  KnowledgeModelSecretChangeDTO
    { name = kmSecret1Edited.name
    , value = kmSecret1Edited.value
    }

kmSecretDifferent :: KnowledgeModelSecret
kmSecretDifferent =
  KnowledgeModelSecret
    { uuid = u' "eb884df9-9ba1-4bf5-85d8-74f3e433cdfb"
    , name = "mySecret1"
    , value = "mySecretValue1"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

workspaceKmSecret1 :: KnowledgeModelSecret
workspaceKmSecret1 =
  KnowledgeModelSecret
    { uuid = u' "c5d6e7f8-9a0b-4c1d-8e2f-3a4b5c6d7e8f"
    , name = "mySecret1"
    , value = "workspaceSecretValue1"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Just defaultWorkspaceUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

secondWorkspaceKmSecret :: KnowledgeModelSecret
secondWorkspaceKmSecret =
  KnowledgeModelSecret
    { uuid = u' "d6e7f8a9-0b1c-4d2e-9f3a-4b5c6d7e8f9a"
    , name = "secondWorkspaceSecret"
    , value = "secondWorkspaceSecretValue"
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Just secondWorkspace.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
