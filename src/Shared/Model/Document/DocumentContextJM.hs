module Shared.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Api.Resource.Report.ReportJM ()
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import Shared.Model.Document.DocumentContext
import Shared.Util.Aeson

instance ToJSON DocumentContext where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextWorkspace where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextProject where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextKnowledgeModelEditor where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUser where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserPerm where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserGroupPerm where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserGroup where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserGroupMember where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextQuestionnaire where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextReport where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextPackage where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocument where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocumentTemplate where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextFormat where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocumentTemplateLocale where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextConfig where
  toJSON = genericToJSON jsonOptions
