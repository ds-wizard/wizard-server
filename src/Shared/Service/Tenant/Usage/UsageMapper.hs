module Shared.Service.Tenant.Usage.UsageMapper where

import GHC.Int

import Shared.Api.Resource.Tenant.Usage.UsageEntryDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Model.Tenant.Limit.TenantLimitBundleChange

toDTO :: TenantLimitBundleChange -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64 -> WizardUsageDTO
toDTO limits userCount activeUserCount knowledgeModelEditorCount kmCount prjCount tmlCount tmlDraftCount docCount storageSize =
  WizardUsageDTO
    { users =
        UsageEntryDTO
          { current = fromIntegral userCount
          , max = fromIntegral limits.users
          }
    , activeUsers =
        UsageEntryDTO
          { current = fromIntegral activeUserCount
          , max = fromIntegral limits.activeUsers
          }
    , knowledgeModels =
        UsageEntryDTO
          { current = fromIntegral kmCount
          , max = fromIntegral limits.knowledgeModels
          }
    , knowledgeModelEditors =
        UsageEntryDTO
          { current = fromIntegral knowledgeModelEditorCount
          , max = fromIntegral limits.knowledgeModelEditors
          }
    , documentTemplates =
        UsageEntryDTO
          { current = fromIntegral tmlCount
          , max = fromIntegral limits.documentTemplates
          }
    , documentTemplateDrafts =
        UsageEntryDTO
          { current = fromIntegral tmlDraftCount
          , max = fromIntegral limits.documentTemplateDrafts
          }
    , projects =
        UsageEntryDTO
          { current = fromIntegral prjCount
          , max = fromIntegral limits.projects
          }
    , documents =
        UsageEntryDTO
          { current = fromIntegral docCount
          , max = fromIntegral limits.documents
          }
    , storage = UsageEntryDTO {current = storageSize, max = limits.storage}
    }
