module Shared.Api.Resource.Document.DocumentContextSM where

import Data.Swagger

import Shared.Api.Resource.Common.SemVer2TupleSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleSM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListSM ()
import Shared.Api.Resource.Report.ReportSM ()
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipSM ()
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.User.User
import Shared.Model.User.UserGroupMembership
import Shared.Service.Document.Context.DocumentContextMapper
import Shared.Util.Swagger

instance ToSchema DocumentContext where
  declareNamedSchema = toSwagger dmp1

instance ToSchema DocumentContextWorkspace where
  declareNamedSchema = toSwagger dmp1.workspace

instance ToSchema DocumentContextProject where
  declareNamedSchema = toSwagger dmp1Project

instance ToSchema DocumentContextKnowledgeModelEditor where
  declareNamedSchema = toSwagger dmpKmEditor1Editor

instance ToSchema DocumentContextUser where
  declareNamedSchema = toSwagger (toDocumentContextUser userAlbert)

instance ToSchema DocumentContextUserPerm where
  declareNamedSchema = toSwagger dmp1AlbertPerm

instance ToSchema DocumentContextUserGroupPerm where
  declareNamedSchema = toSwagger DocumentContextUserGroupPerm {group = toDocumentContextUserGroup bioGroup [], perms = ["VIEW"]}

instance ToSchema DocumentContextUserGroup where
  declareNamedSchema = toSwagger (toDocumentContextUserGroup bioGroup [])

instance ToSchema DocumentContextUserGroupMember where
  declareNamedSchema =
    toSwagger
      DocumentContextUserGroupMember
        { uuid = userAlbert.uuid
        , firstName = userAlbert.firstName
        , lastName = userAlbert.lastName
        , gravatarHash = ""
        , imageUrl = userAlbert.imageUrl
        , affiliation = userAlbert.affiliation
        , membershipType = OwnerUserGroupMembershipType
        }

instance ToSchema DocumentContextQuestionnaire where
  declareNamedSchema = toSwagger dmp1Questionnaire

instance ToSchema DocumentContextReport where
  declareNamedSchema = toSwagger dmp1Questionnaire.report

instance ToSchema DocumentContextPackage where
  declareNamedSchema = toSwagger dmp1Package

instance ToSchema DocumentContextDocument where
  declareNamedSchema = toSwagger dmp1Document

instance ToSchema DocumentContextDocumentTemplate where
  declareNamedSchema = toSwagger dmp1Document.documentTemplate

instance ToSchema DocumentContextFormat where
  declareNamedSchema = toSwagger dmp1Document.format

instance ToSchema DocumentContextDocumentTemplateLocale where
  declareNamedSchema = toSwagger (toDocumentContextDocumentTemplateLocale czechWizardDocumentTemplateLocale)

instance ToSchema DocumentContextConfig where
  declareNamedSchema = toSwagger dmp1.config
