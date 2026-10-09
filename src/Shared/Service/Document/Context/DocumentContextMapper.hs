module Shared.Service.Document.Context.DocumentContextMapper where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import qualified Shared.Constant.DocumentTemplate as TemplateConstant
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.Report.Report
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserWithMembership
import Shared.Model.Workspace.Workspace
import Shared.Util.Gravatar (createGravatarHash)

toProjectDocumentContext
  :: Workspace
  -> Project
  -> Maybe User
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> ProjectContent
  -> Maybe U.UUID
  -> [ProjectVersionList]
  -> [ProjectFileSimple]
  -> Report
  -> KnowledgeModel
  -> KnowledgeModelPackage
  -> DocumentContextConfig
  -> DocumentContext
toProjectDocumentContext workspace project mCreatedBy users groups content mVersionUuid versions files report km pkg config =
  DocumentContext
    { metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , hash = ""
    , workspace = toDocumentContextWorkspace workspace
    , project = Just $ toDocumentContextProject project mCreatedBy users groups
    , knowledgeModelEditor = Nothing
    , questionnaire =
        DocumentContextQuestionnaire
          { replies = content.replies
          , labels = content.labels
          , phaseUuid = content.phaseUuid
          , versionUuid = mVersionUuid
          , versions = L.sortOn (\v -> (v.createdAt, v.uuid)) versions
          , files = L.sortOn (.uuid) files
          , report = toDocumentContextReport report
          , language = project.language
          }
    , knowledgeModel = km
    , knowledgeModelPackage = Just $ toDocumentContextPackage pkg
    , document = Nothing
    , config = config
    }

toKnowledgeModelEditorDocumentContext
  :: Workspace
  -> KnowledgeModelEditor
  -> Maybe KnowledgeModelPackage
  -> Maybe User
  -> M.Map String Reply
  -> Report
  -> KnowledgeModel
  -> DocumentContextConfig
  -> DocumentContext
toKnowledgeModelEditorDocumentContext workspace editor mPreviousPkg mCreatedBy replies report km config =
  DocumentContext
    { metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , hash = ""
    , workspace = toDocumentContextWorkspace workspace
    , project = Nothing
    , knowledgeModelEditor = Just $ toDocumentContextKnowledgeModelEditor editor mPreviousPkg mCreatedBy
    , questionnaire =
        DocumentContextQuestionnaire
          { replies = replies
          , labels = M.empty
          , phaseUuid = Nothing
          , versionUuid = Nothing
          , versions = []
          , files = []
          , report = toDocumentContextReport report
          , language = if null editor.language then Nothing else Just editor.language
          }
    , knowledgeModel = km
    , knowledgeModelPackage = fmap toDocumentContextPackage mPreviousPkg
    , document = Nothing
    , config = config
    }

toDocumentContextWorkspace :: Workspace -> DocumentContextWorkspace
toDocumentContextWorkspace workspace =
  DocumentContextWorkspace
    { uuid = workspace.uuid
    , name = workspace.name
    , description = workspace.description
    }

toDocumentContextProject :: Project -> Maybe User -> [DocumentContextUserPerm] -> [DocumentContextUserGroupPerm] -> DocumentContextProject
toDocumentContextProject project mCreatedBy users groups =
  DocumentContextProject
    { uuid = project.uuid
    , name = project.name
    , description = project.description
    , createdBy = toDocumentContextUser <$> mCreatedBy
    , createdAt = project.createdAt
    , updatedAt = project.updatedAt
    , visibility = project.visibility
    , sharing = project.sharing
    , isTemplate = project.isTemplate
    , knowledgeModelPackageUuid = project.knowledgeModelPackageUuid
    , selectedQuestionTagUuids = project.selectedQuestionTagUuids
    , projectTags = project.projectTags
    , documentTemplateUuid = project.documentTemplateUuid
    , formatUuid = project.formatUuid
    , users = L.sortOn (\u -> u.user.uuid) users
    , groups = L.sortOn (\g -> g.group.uuid) groups
    }

toDocumentContextKnowledgeModelEditor :: KnowledgeModelEditor -> Maybe KnowledgeModelPackage -> Maybe User -> DocumentContextKnowledgeModelEditor
toDocumentContextKnowledgeModelEditor editor mPreviousPkg mCreatedBy =
  DocumentContextKnowledgeModelEditor
    { uuid = editor.uuid
    , name = editor.name
    , description = editor.description
    , createdBy = toDocumentContextUser <$> mCreatedBy
    , createdAt = editor.createdAt
    , updatedAt = editor.updatedAt
    , dckeId = editor.id
    , version = editor.version
    , license = editor.license
    , readme = editor.readme
    , previousPackageUuid = editor.previousPackageUuid
    , previousPackageId = fmap (.id) mPreviousPkg
    , previousPackageVersion = fmap (.version) mPreviousPkg
    , dckeMetamodelVersion = editor.metamodelVersion
    }

toDocumentContextReport :: Report -> DocumentContextReport
toDocumentContextReport report =
  DocumentContextReport
    { totalReport = report.totalReport
    , chapterReports = report.chapterReports
    , chapters = report.chapters
    , metrics = report.metrics
    }

toDocumentContextPackage :: KnowledgeModelPackage -> DocumentContextPackage
toDocumentContextPackage pkg =
  DocumentContextPackage
    { uuid = pkg.uuid
    , dcpId = pkg.id
    , version = pkg.version
    , name = pkg.name
    , description = pkg.description
    , language = pkg.language
    , createdAt = pkg.createdAt
    , workspaceUuid = pkg.workspaceUuid
    }

toDocumentContextDocument :: Document -> DocumentTemplate -> DocumentTemplateFormatSimple -> Maybe User -> Maybe DocumentTemplateLocale -> DocumentContextDocument
toDocumentContextDocument doc dt format mCreatedBy mLocale =
  DocumentContextDocument
    { uuid = doc.uuid
    , name = doc.name
    , documentTemplate = toDocumentContextDocumentTemplate dt
    , format = DocumentContextFormat {uuid = format.uuid, name = format.name}
    , language = doc.language
    , locale = toDocumentContextDocumentTemplateLocale <$> mLocale
    , preview = doc.durability == TemporallyDocumentDurability
    , eventUuid = doc.projectEventUuid
    , createdBy = toDocumentContextUser <$> mCreatedBy
    , createdAt = doc.createdAt
    }

toDocumentContextDocumentTemplate :: DocumentTemplate -> DocumentContextDocumentTemplate
toDocumentContextDocumentTemplate dt =
  DocumentContextDocumentTemplate
    { uuid = dt.uuid
    , dcdtId = dt.id
    , version = dt.version
    , workspaceUuid = dt.workspaceUuid
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

toDocumentContextUserGroup :: UserGroup -> [UserWithMembership] -> DocumentContextUserGroup
toDocumentContextUserGroup userGroup members =
  DocumentContextUserGroup
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    , members = L.sortOn (.uuid) (fmap toDocumentContextUserGroupMember members)
    , createdAt = userGroup.createdAt
    , updatedAt = userGroup.updatedAt
    }

toDocumentContextUserGroupMember :: UserWithMembership -> DocumentContextUserGroupMember
toDocumentContextUserGroupMember member =
  DocumentContextUserGroupMember
    { uuid = member.uuid
    , firstName = member.firstName
    , lastName = member.lastName
    , gravatarHash = createGravatarHash member.email
    , imageUrl = member.imageUrl
    , affiliation = member.affiliation
    , membershipType = member.membershipType
    }
