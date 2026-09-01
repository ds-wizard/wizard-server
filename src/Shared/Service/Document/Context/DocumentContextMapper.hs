module Shared.Service.Document.Context.DocumentContextMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import qualified Shared.Constant.DocumentTemplate as TemplateConstant
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.Report.Report
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.User
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper

toDocumentContext
  :: Document
  -> String
  -> Project
  -> Maybe U.UUID
  -> M.Map String Reply
  -> M.Map String [U.UUID]
  -> Maybe ProjectVersion
  -> [ProjectVersionList]
  -> [ProjectFileSimple]
  -> KnowledgeModel
  -> DocumentTemplate
  -> Report
  -> KnowledgeModelPackage
  -> TenantConfigOrganization
  -> TenantConfigLookAndFeel
  -> Maybe User
  -> Maybe User
  -> Maybe DocumentTemplateLocale
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> DocumentContext
toDocumentContext doc appClientUrl project phaseUuid replies labels mProjectVersion projectVersionDtos projectFiles km dt report pkg org lookAndFeel mProjectCreatedBy mDocCreatedBy mLocale users groups =
  DocumentContext
    { config =
        DocumentContextConfig
          { clientUrl = appClientUrl
          , appTitle = lookAndFeel.appTitle
          , appTitleShort = lookAndFeel.appTitleShort
          , illustrationsColor = lookAndFeel.illustrationsColor
          , primaryColor = lookAndFeel.primaryColor
          , logoUrl = lookAndFeel.logoUrl
          }
    , document =
        DocumentContextDocument
          { uuid = doc.uuid
          , name = doc.name
          , documentTemplateUuid = doc.documentTemplateUuid
          , formatUuid = doc.formatUuid
          , language = doc.language
          , locale = toDocumentContextDocumentTemplateLocale <$> mLocale
          , createdBy = toDocumentContextUser <$> mDocCreatedBy
          , createdAt = doc.createdAt
          }
    , project =
        DocumentContextProject
          { uuid = project.uuid
          , name = project.name
          , description = project.description
          , replies = replies
          , phaseUuid = phaseUuid
          , labels = labels
          , versionUuid = fmap (.uuid) mProjectVersion
          , versions = projectVersionDtos
          , projectTags = project.projectTags
          , files = projectFiles
          , language = project.language
          , createdBy = toDocumentContextUser <$> mProjectCreatedBy
          , createdAt = project.createdAt
          , updatedAt = project.updatedAt
          }
    , knowledgeModel = km
    , report = report
    , knowledgeModelPackage = toDocumentContextPackage pkg
    , organization = org
    , metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , users = users
    , groups = groups
    }

toDocumentContextPackage :: KnowledgeModelPackage -> DocumentContextPackage
toDocumentContextPackage pkg =
  let dto = toSimpleDTO pkg
   in DocumentContextPackage
        { uuid = pkg.uuid
        , name = dto.name
        , organizationId = dto.organizationId
        , kmId = pkg.kmId
        , version = dto.version
        , versions = []
        , remoteLatestVersion = dto.remoteLatestVersion
        , description = dto.description
        , organization = dto.organization
        , language = dto.language
        , createdAt = dto.createdAt
        }

toDocumentContextDocumentTemplateLocale :: DocumentTemplateLocale -> DocumentContextDocumentTemplateLocale
toDocumentContextDocumentTemplateLocale locale =
  DocumentContextDocumentTemplateLocale
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toDocumentContextUser :: User -> DocumentContextUser
toDocumentContextUser user =
  DocumentContextUser
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , affiliation = user.affiliation
    , active = user.active
    , imageUrl = user.imageUrl
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    }
