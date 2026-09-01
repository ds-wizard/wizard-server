module Shared.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Api.Resource.Report.ReportJM ()
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Api.Resource.User.UserJM ()
import Shared.Model.Document.DocumentContext
import Shared.Util.Aeson

instance ToJSON DocumentContext where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextConfig where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUser where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextPackage where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextProject where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocument where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocumentTemplateLocale where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserPerm where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserGroupPerm where
  toJSON = genericToJSON jsonOptions
