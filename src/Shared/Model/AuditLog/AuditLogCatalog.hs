module Shared.Model.AuditLog.AuditLogCatalog where

import Shared.Model.AuditLog.AuditLogEntityType
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.AuditLogEventDefinition
import Shared.Model.AuditLog.AuditLogEventDefinition.Internal
import Shared.Model.AuditLog.AuditLogScope
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.AuditLog.Data.LocaleAuditLogData
import Shared.Model.AuditLog.Data.ProjectAuditLogData
import Shared.Model.AuditLog.Data.SettingsAuditLogData
import Shared.Model.AuditLog.Data.UserAuditLogData
import Shared.Model.AuditLog.Data.UserGroupAuditLogData
import Shared.Model.AuditLog.Data.WorkspaceAuditLogData
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Util.Date
import Shared.Util.Uuid

sharedAuditLogCatalog :: [SomeAuditLogEventDefinition]
sharedAuditLogCatalog =
  [ SomeAuditLogEventDefinition workspacesCreate
  , SomeAuditLogEventDefinition workspacesUpdate
  , SomeAuditLogEventDefinition workspacesDelete
  , SomeAuditLogEventDefinition workspacesMemberAdd
  , SomeAuditLogEventDefinition workspacesMemberRemove
  , SomeAuditLogEventDefinition workspacesMemberRoleChange
  , SomeAuditLogEventDefinition usersCreateByAdmin
  , SomeAuditLogEventDefinition userGroupsCreate
  , SomeAuditLogEventDefinition userGroupsUpdate
  , SomeAuditLogEventDefinition userGroupsDelete
  , SomeAuditLogEventDefinition userGroupsUpdateMembers
  , SomeAuditLogEventDefinition settingsUpdate
  , SomeAuditLogEventDefinition settingsOverrideSet
  , SomeAuditLogEventDefinition settingsOverrideReset
  , SomeAuditLogEventDefinition settingsOverrideAllowedChange
  , SomeAuditLogEventDefinition settingsLogoUpload
  , SomeAuditLogEventDefinition settingsLogoDelete
  , SomeAuditLogEventDefinition pluginsEnable
  , SomeAuditLogEventDefinition pluginsDisable
  , SomeAuditLogEventDefinition pluginsConfigure
  , SomeAuditLogEventDefinition pluginsOverrideAllowedChange
  , SomeAuditLogEventDefinition pluginsOverrideReset
  , SomeAuditLogEventDefinition localesCreate
  , SomeAuditLogEventDefinition localesReplace
  , SomeAuditLogEventDefinition localesUpdate
  , SomeAuditLogEventDefinition localesDelete
  , SomeAuditLogEventDefinition localesExport
  , SomeAuditLogEventDefinition mailTemplateLocalesCreate
  , SomeAuditLogEventDefinition mailTemplateLocalesReplace
  , SomeAuditLogEventDefinition mailTemplateLocalesUpdate
  , SomeAuditLogEventDefinition mailTemplateLocalesDelete
  , SomeAuditLogEventDefinition mailTemplateLocalesExport
  , SomeAuditLogEventDefinition knowledgeModelsImportFromFile
  , SomeAuditLogEventDefinition knowledgeModelsPullFromRegistry
  , SomeAuditLogEventDefinition knowledgeModelsExport
  , SomeAuditLogEventDefinition knowledgeModelsPublish
  , SomeAuditLogEventDefinition knowledgeModelsDeprecate
  , SomeAuditLogEventDefinition knowledgeModelsRestore
  , SomeAuditLogEventDefinition knowledgeModelsSetVisibility
  , SomeAuditLogEventDefinition knowledgeModelsDelete
  , SomeAuditLogEventDefinition knowledgeModelEditorsCreate
  , SomeAuditLogEventDefinition knowledgeModelEditorsUpdate
  , SomeAuditLogEventDefinition knowledgeModelEditorsDelete
  , SomeAuditLogEventDefinition knowledgeModelEditorsMigrationStart
  , SomeAuditLogEventDefinition knowledgeModelEditorsMigrationFinish
  , SomeAuditLogEventDefinition knowledgeModelEditorsMigrationCancel
  , SomeAuditLogEventDefinition knowledgeModelSecretsCreate
  , SomeAuditLogEventDefinition knowledgeModelSecretsUpdate
  , SomeAuditLogEventDefinition knowledgeModelSecretsDelete
  , SomeAuditLogEventDefinition documentTemplatesImportFromFile
  , SomeAuditLogEventDefinition documentTemplatesPullFromRegistry
  , SomeAuditLogEventDefinition documentTemplatesExport
  , SomeAuditLogEventDefinition documentTemplatesPublish
  , SomeAuditLogEventDefinition documentTemplatesDeprecate
  , SomeAuditLogEventDefinition documentTemplatesRestore
  , SomeAuditLogEventDefinition documentTemplatesDelete
  , SomeAuditLogEventDefinition documentTemplateEditorsCreate
  , SomeAuditLogEventDefinition documentTemplateEditorsUpdate
  , SomeAuditLogEventDefinition documentTemplateEditorsDelete
  , SomeAuditLogEventDefinition projectsCreate
  , SomeAuditLogEventDefinition projectsClone
  , SomeAuditLogEventDefinition projectsImport
  , SomeAuditLogEventDefinition projectsUpdate
  , SomeAuditLogEventDefinition projectsPermissionsUpdate
  , SomeAuditLogEventDefinition projectsVersionCreate
  , SomeAuditLogEventDefinition projectsVersionUpdate
  , SomeAuditLogEventDefinition projectsVersionDelete
  , SomeAuditLogEventDefinition projectsRevert
  , SomeAuditLogEventDefinition projectsMigrate
  , SomeAuditLogEventDefinition projectsDelete
  , SomeAuditLogEventDefinition documentsCreate
  , SomeAuditLogEventDefinition documentsDelete
  , SomeAuditLogEventDefinition documentsSubmit
  , SomeAuditLogEventDefinition projectFilesUpload
  , SomeAuditLogEventDefinition projectFilesDelete
  ]

-- ---------------------------------------------------------------------------------------------------------------------
workspacesCreate :: AuditLogEventDefinition () AuditLogWorkspaceData
workspacesCreate =
  workspaceEvent "create" "A workspace was created." "The created workspace." AuditLogWorkspaceData {description = Just "Everyone in the physics department.", primaryColor = Nothing}

workspacesUpdate :: AuditLogEventDefinition AuditLogWorkspaceChanges NoData
workspacesUpdate =
  withChangesExample AuditLogWorkspaceChanges {name = change "Physics" "Physics Department", description = Nothing, primaryColor = change Nothing (Just "#019ad6")} $
    workspaceEvent "update" "Workspace details changed. The changed fields are in changes." "The changed workspace; entity.name is the name after the change." NoData

workspacesDelete :: AuditLogEventDefinition () AuditLogWorkspaceDeleteData
workspacesDelete =
  workspaceEvent
    "delete"
    "A workspace was deleted together with everything in it; the data counts what was deleted with it."
    "The deleted workspace."
    AuditLogWorkspaceDeleteData {projects = 12, documents = 30, knowledgeModelEditors = 2, documentTemplateEditors = 1, userGroups = 3}

workspacesMemberAdd :: AuditLogEventDefinition () AuditLogWorkspaceMemberData
workspacesMemberAdd =
  workspaceEvent "memberAdd" "A user became a member of a workspace, by a request or by a login automation." "The workspace the member was added to." AuditLogWorkspaceMemberData {member = exampleUser, role = exampleWorkspaceRole}

workspacesMemberRemove :: AuditLogEventDefinition () AuditLogWorkspaceMemberData
workspacesMemberRemove =
  workspaceEvent "memberRemove" "A member was removed from a workspace." "The workspace the member was removed from." AuditLogWorkspaceMemberData {member = exampleUser, role = exampleWorkspaceRole}

workspacesMemberRoleChange :: AuditLogEventDefinition () AuditLogWorkspaceMemberRoleChangeData
workspacesMemberRoleChange =
  workspaceEvent
    "memberRoleChange"
    "The workspace role of a member changed."
    "The workspace of the member."
    AuditLogWorkspaceMemberRoleChangeData {member = exampleUser, oldRole = exampleWorkspaceRole, newRole = AuditLogRef {uuid = u' "0b6f2d4e-8c1a-4e7b-9d3f-5a2c7e9b1d40", name = "Admin"}}

workspaceEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
workspaceEvent action description = auditLogEvent "workspaces" action description WorkspaceAuditLogScope True WorkspaceAuditLogEntityType

exampleWorkspaceRole :: AuditLogRef
exampleWorkspaceRole = AuditLogRef {uuid = u' "9e3b5c7d-1f2a-4b6c-8d0e-3a5b7c9d1e2f", name = "User"}

-- ---------------------------------------------------------------------------------------------------------------------
usersCreateByAdmin :: AuditLogEventDefinition () AuditLogUserData
usersCreateByAdmin =
  auditLogEvent "users" "createByAdmin" "An administrator created a user." OrganizationAuditLogScope True UserAuditLogEntityType "The created user; entity.name is the email." exampleUserData

exampleUserData :: AuditLogUserData
exampleUserData = AuditLogUserData {firstName = "Sofia", lastName = "Novak", affiliation = Just "Physics Department", role = exampleRole}

exampleRole :: AuditLogRef
exampleRole = AuditLogRef {uuid = u' "c4d2e8a1-7b35-4f90-a6e3-1d8f2c5b7a09", name = "Researcher"}

-- ---------------------------------------------------------------------------------------------------------------------
userGroupsCreate :: AuditLogEventDefinition () AuditLogUserGroupData
userGroupsCreate =
  auditLogEvent "userGroups" "create" "A user group was created in a workspace." WorkspaceAuditLogScope True UserGroupAuditLogEntityType "The created user group." exampleUserGroupData

userGroupsUpdate :: AuditLogEventDefinition AuditLogUserGroupChanges NoData
userGroupsUpdate =
  withChangesExample AuditLogUserGroupChanges {name = Nothing, description = change (Just "Lab members") Nothing, private = change False True} $
    auditLogEvent "userGroups" "update" "User group settings changed. The changed fields are in changes." WorkspaceAuditLogScope True UserGroupAuditLogEntityType "The changed user group; entity.name is the name after the change." NoData

userGroupsDelete :: AuditLogEventDefinition () AuditLogUserGroupData
userGroupsDelete =
  auditLogEvent "userGroups" "delete" "A user group was deleted." WorkspaceAuditLogScope True UserGroupAuditLogEntityType "The deleted user group." exampleUserGroupData

userGroupsUpdateMembers :: AuditLogEventDefinition () AuditLogUserGroupMembersData
userGroupsUpdateMembers =
  auditLogEvent
    "userGroups"
    "updateMembers"
    "Members of a user group were added, removed or got another membership type."
    WorkspaceAuditLogScope
    True
    UserGroupAuditLogEntityType
    "The user group whose members changed."
    AuditLogUserGroupMembersData {add = [AuditLogUserGroupMember {user = exampleUser, membershipType = "MemberUserGroupMembershipType"}], edit = [], delete = []}

exampleUserGroupData :: AuditLogUserGroupData
exampleUserGroupData = AuditLogUserGroupData {description = Just "Everyone working on genomics.", private = False}

exampleUser :: AuditLogUserRef
exampleUser =
  AuditLogUserRef
    { uuid = u' "e1a9f3c7-2d48-4b6e-9f05-8c3b7a2d1e46"
    , firstName = "Sofia"
    , lastName = "Novak"
    , email = "sofia.novak@example.org"
    , imageUrl = Nothing
    }

-- ---------------------------------------------------------------------------------------------------------------------
pluginsEnable :: AuditLogEventDefinition () NoData
pluginsEnable =
  auditLogEvent "plugins" "enable" "A plugin was enabled for the organization or for a workspace." OrganizationOrWorkspaceAuditLogScope True PluginAuditLogEntityType "The enabled plugin; entity.name is its URL." NoData

pluginsDisable :: AuditLogEventDefinition () NoData
pluginsDisable =
  auditLogEvent "plugins" "disable" "A plugin was disabled for the organization or for a workspace." OrganizationOrWorkspaceAuditLogScope True PluginAuditLogEntityType "The disabled plugin; entity.name is its URL." NoData

pluginsConfigure :: AuditLogEventDefinition () NoData
pluginsConfigure =
  auditLogEvent "plugins" "configure" "The settings of a plugin were saved. The values are never recorded." OrganizationOrWorkspaceAuditLogScope True PluginAuditLogEntityType "The configured plugin; entity.name is its URL." NoData

pluginsOverrideAllowedChange :: AuditLogEventDefinition AuditLogOverrideAllowedChanges NoData
pluginsOverrideAllowedChange =
  withChangesExample exampleOverrideAllowedChanges $
    auditLogEvent "plugins" "overrideAllowedChange" "Workspaces were allowed or forbidden to override the settings of a plugin." OrganizationAuditLogScope True PluginAuditLogEntityType "The plugin whose override permission changed; entity.name is its URL." NoData

pluginsOverrideReset :: AuditLogEventDefinition () NoData
pluginsOverrideReset =
  auditLogEvent "plugins" "overrideReset" "A workspace dropped its override of the settings of a plugin." WorkspaceAuditLogScope True PluginAuditLogEntityType "The plugin whose workspace override was reset; entity.name is its URL." NoData

-- ---------------------------------------------------------------------------------------------------------------------
settingsUpdate :: AuditLogEventDefinition AuditLogSettingsChanges AuditLogSettingsUpdateData
settingsUpdate =
  withChangesExample exampleSettingsChanges $
    settingsEvent
      "update"
      "A section of the settings changed. The changed fields of the section are in changes; submission request headers and the registry token are never recorded, apiKeyChanged (registry only) says whether the token changed."
      OrganizationOrWorkspaceAuditLogScope
      AuditLogSettingsUpdateData {section = "projects", apiKeyChanged = Nothing}

settingsOverrideSet :: AuditLogEventDefinition AuditLogSettingsChanges AuditLogSettingsData
settingsOverrideSet =
  withChangesExample exampleSettingsChanges $
    settingsEvent
      "overrideSet"
      "A workspace saved its own values of a section. changes compares them with the values the workspace used before."
      WorkspaceAuditLogScope
      exampleSettingsData

settingsOverrideReset :: AuditLogEventDefinition () AuditLogSettingsData
settingsOverrideReset =
  settingsEvent "overrideReset" "A workspace dropped its own values of a section and uses the organization values again." WorkspaceAuditLogScope exampleSettingsData

settingsOverrideAllowedChange :: AuditLogEventDefinition AuditLogOverrideAllowedChanges AuditLogSettingsData
settingsOverrideAllowedChange =
  withChangesExample exampleOverrideAllowedChanges $
    settingsEvent "overrideAllowedChange" "Workspaces were allowed or forbidden to override a section." OrganizationAuditLogScope exampleSettingsData

settingsLogoUpload :: AuditLogEventDefinition () AuditLogSettingsData
settingsLogoUpload =
  settingsEvent "logoUpload" "A logo was uploaded for the organization or for a workspace." OrganizationOrWorkspaceAuditLogScope AuditLogSettingsData {section = "lookAndFeel"}

settingsLogoDelete :: AuditLogEventDefinition () AuditLogSettingsData
settingsLogoDelete =
  settingsEvent "logoDelete" "The logo of the organization or of a workspace was removed." OrganizationOrWorkspaceAuditLogScope AuditLogSettingsData {section = "lookAndFeel"}

settingsEvent :: String -> String -> AuditLogScope -> d -> AuditLogEventDefinition c d
settingsEvent action description scope =
  uncurry (auditLogEvent "settings" action description scope True) (settingsEntityOf scope)

settingsEntityOf :: AuditLogScope -> (AuditLogEntityType, String)
settingsEntityOf OrganizationAuditLogScope = (OrganizationAuditLogEntityType, "The organization.")
settingsEntityOf WorkspaceAuditLogScope = (WorkspaceAuditLogEntityType, "The workspace whose own values changed.")
settingsEntityOf OrganizationOrWorkspaceAuditLogScope = (OrganizationAuditLogEntityType, "The organization, or the workspace when entity.workspaceUuid is set.")

exampleSettingsData :: AuditLogSettingsData
exampleSettingsData = AuditLogSettingsData {section = "projects"}

exampleSettingsChanges :: AuditLogSettingsChanges
exampleSettingsChanges =
  noSettingsChanges {projectVisibility = change (projectsVisibility PrivateProjectVisibility) (projectsVisibility VisibleViewProjectVisibility)} :: AuditLogSettingsChanges

projectsVisibility :: ProjectVisibility -> SettingsProjectsVisibility
projectsVisibility visibility = SettingsProjectsVisibility {enabled = True, defaultValue = visibility}

exampleOverrideAllowedChanges :: AuditLogOverrideAllowedChanges
exampleOverrideAllowedChanges = AuditLogOverrideAllowedChanges {workspaceOverrideAllowed = change False True}

-- ---------------------------------------------------------------------------------------------------------------------
localesCreate :: AuditLogEventDefinition () AuditLogLocaleData
localesCreate = localeEvent "create" "A UI locale was uploaded for a language the organization did not have yet." True "The created locale." exampleLocaleData

localesReplace :: AuditLogEventDefinition () AuditLogLocaleReplaceData
localesReplace =
  localeEvent
    "replace"
    "The PO file of a UI locale was replaced by an upload; officialSwitchedOff says whether the locale stopped being official."
    True
    "The replaced locale."
    AuditLogLocaleReplaceData {code = "de", name = "Deutsch", oldPotVersion = Just "fw-client:4.31.0", newPotVersion = Just "fw-client:5.0.0", officialSwitchedOff = True}

localesUpdate :: AuditLogEventDefinition AuditLogLocaleChanges NoData
localesUpdate =
  withChangesExample AuditLogLocaleChanges {enabled = change False True, defaultLocale = Nothing} $
    localeEvent "update" "A UI locale was enabled, disabled or made the default. The changed fields are in changes." True "The changed locale." NoData

localesDelete :: AuditLogEventDefinition () AuditLogLocaleData
localesDelete = localeEvent "delete" "A UI locale was deleted." True "The deleted locale." exampleLocaleData

localesExport :: AuditLogEventDefinition () AuditLogLocaleData
localesExport = localeEvent "export" "The PO file of a UI locale was downloaded." False "The exported locale." exampleLocaleData

localeEvent :: String -> String -> Bool -> String -> d -> AuditLogEventDefinition c d
localeEvent action description eventBridge =
  auditLogEvent "locales" action description OrganizationAuditLogScope eventBridge LocaleAuditLogEntityType

exampleLocaleData :: AuditLogLocaleData
exampleLocaleData = AuditLogLocaleData {code = "de", name = "Deutsch", potVersion = Just "fw-client:5.0.0", official = True}

-- ---------------------------------------------------------------------------------------------------------------------
mailTemplateLocalesCreate :: AuditLogEventDefinition () AuditLogLocaleData
mailTemplateLocalesCreate =
  mailTemplateLocaleEvent
    "create"
    "A mail template locale was uploaded for a language the organization did not have yet."
    True
    "The created mail template locale."
    exampleMailTemplateLocaleData

mailTemplateLocalesReplace :: AuditLogEventDefinition () AuditLogLocaleReplaceData
mailTemplateLocalesReplace =
  mailTemplateLocaleEvent
    "replace"
    "The PO file of a mail template locale was replaced by an upload; officialSwitchedOff says whether the locale stopped being official."
    True
    "The replaced mail template locale."
    AuditLogLocaleReplaceData
      { code = "de"
      , name = "Deutsch"
      , oldPotVersion = Just "fw-mail-templates:4.31.0"
      , newPotVersion = Just "fw-mail-templates:5.0.0"
      , officialSwitchedOff = True
      }

mailTemplateLocalesUpdate :: AuditLogEventDefinition AuditLogLocaleChanges NoData
mailTemplateLocalesUpdate =
  withChangesExample AuditLogLocaleChanges {enabled = change False True, defaultLocale = Nothing} $
    mailTemplateLocaleEvent
      "update"
      "A mail template locale was enabled, disabled or made the default. The changed fields are in changes."
      True
      "The changed mail template locale."
      NoData

mailTemplateLocalesDelete :: AuditLogEventDefinition () AuditLogLocaleData
mailTemplateLocalesDelete =
  mailTemplateLocaleEvent "delete" "A mail template locale was deleted." True "The deleted mail template locale." exampleMailTemplateLocaleData

mailTemplateLocalesExport :: AuditLogEventDefinition () AuditLogLocaleData
mailTemplateLocalesExport =
  mailTemplateLocaleEvent
    "export"
    "The PO file of a mail template locale was downloaded."
    False
    "The exported mail template locale."
    exampleMailTemplateLocaleData

mailTemplateLocaleEvent :: String -> String -> Bool -> String -> d -> AuditLogEventDefinition c d
mailTemplateLocaleEvent action description eventBridge =
  auditLogEvent "mailTemplateLocales" action description OrganizationAuditLogScope eventBridge MailTemplateLocaleAuditLogEntityType

exampleMailTemplateLocaleData :: AuditLogLocaleData
exampleMailTemplateLocaleData = AuditLogLocaleData {code = "de", name = "Deutsch", potVersion = Just "fw-mail-templates:5.0.0", official = True}

-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelsImportFromFile :: AuditLogEventDefinition () AuditLogLibraryData
knowledgeModelsImportFromFile =
  knowledgeModelEvent "importFromFile" "A knowledge model bundle was imported from a file." True "The imported package (the newest version of the bundle)." exampleKnowledgeModelData

knowledgeModelsPullFromRegistry :: AuditLogEventDefinition () AuditLogLibraryData
knowledgeModelsPullFromRegistry =
  knowledgeModelEvent "pullFromRegistry" "A knowledge model bundle was pulled from the registry." True "The pulled package (the newest version of the bundle)." exampleKnowledgeModelData

knowledgeModelsExport :: AuditLogEventDefinition () AuditLogLibraryData
knowledgeModelsExport =
  knowledgeModelEvent "export" "A knowledge model bundle was exported to a file." False "The exported package." exampleKnowledgeModelData

knowledgeModelsPublish :: AuditLogEventDefinition () AuditLogLibraryPublishData
knowledgeModelsPublish =
  knowledgeModelEvent
    "publish"
    "A knowledge model editor was published as a new package version, directly or by finishing a migration."
    True
    "The published package."
    AuditLogLibraryPublishData {id = exampleKnowledgeModelData.id, version = exampleKnowledgeModelData.version, editor = exampleKnowledgeModelEditor}

knowledgeModelsDeprecate :: AuditLogEventDefinition () AuditLogLibraryData
knowledgeModelsDeprecate = knowledgeModelEvent "deprecate" "A knowledge model package was deprecated." True "The deprecated package." exampleKnowledgeModelData

knowledgeModelsRestore :: AuditLogEventDefinition () AuditLogLibraryData
knowledgeModelsRestore = knowledgeModelEvent "restore" "A deprecated knowledge model package was released again." True "The restored package." exampleKnowledgeModelData

knowledgeModelsSetVisibility :: AuditLogEventDefinition AuditLogKnowledgeModelVisibilityChanges AuditLogLibraryData
knowledgeModelsSetVisibility =
  withChangesExample AuditLogKnowledgeModelVisibilityChanges {public = change False True} $
    knowledgeModelEvent "setVisibility" "A knowledge model package was made public or private. The change is in changes." True "The changed package." exampleKnowledgeModelData

knowledgeModelsDelete :: AuditLogEventDefinition () AuditLogLibraryDeleteData
knowledgeModelsDelete =
  knowledgeModelEvent
    "delete"
    "A knowledge model package was deleted, one version or all versions; dependents counts what depended on the deleted versions."
    True
    "The package the delete was requested for."
    AuditLogLibraryDeleteData {allVersions = True, deletedPackages = ["myorg:genomics:1.0.0", "myorg:genomics:1.1.0"], dependents = exampleDependents}

knowledgeModelEvent :: String -> String -> Bool -> String -> d -> AuditLogEventDefinition c d
knowledgeModelEvent action description eventBridge =
  auditLogEvent "knowledgeModels" action description OrganizationOrWorkspaceAuditLogScope eventBridge KnowledgeModelAuditLogEntityType

exampleKnowledgeModelData :: AuditLogLibraryData
exampleKnowledgeModelData = AuditLogLibraryData {id = "myorg:genomics", version = "1.1.0"}

exampleKnowledgeModel :: AuditLogKnowledgeModelRef
exampleKnowledgeModel =
  AuditLogKnowledgeModelRef {uuid = u' "5c2f7a1e-3b9d-4e6a-8f10-2d4c6e8a0b13", id = "myorg:genomics", version = "1.0.0", name = "Genomics"}

exampleKnowledgeModelEditor :: AuditLogRef
exampleKnowledgeModelEditor = AuditLogRef {uuid = u' "7a1d3f5b-9c2e-4b8a-a6d0-1e3f5a7c9b24", name = "Genomics"}

exampleDependents :: AuditLogLibraryDependentsData
exampleDependents = AuditLogLibraryDependentsData {editors = 1, projects = 4, documents = 9}

-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelEditorsCreate :: AuditLogEventDefinition () AuditLogKnowledgeModelEditorData
knowledgeModelEditorsCreate =
  knowledgeModelEditorEvent "create" "A knowledge model editor was created, empty or based on a package." "The created editor." exampleKnowledgeModelEditorData

knowledgeModelEditorsUpdate :: AuditLogEventDefinition AuditLogEditorChanges NoData
knowledgeModelEditorsUpdate =
  withChangesExample exampleEditorChanges $
    knowledgeModelEditorEvent "update" "The details of a knowledge model editor changed. The changed fields are in changes." "The changed editor; entity.name is the name after the change." NoData

knowledgeModelEditorsDelete :: AuditLogEventDefinition () AuditLogKnowledgeModelEditorData
knowledgeModelEditorsDelete = knowledgeModelEditorEvent "delete" "A knowledge model editor was deleted." "The deleted editor." exampleKnowledgeModelEditorData

knowledgeModelEditorsMigrationStart :: AuditLogEventDefinition () AuditLogKnowledgeModelMigrationData
knowledgeModelEditorsMigrationStart =
  knowledgeModelEditorEvent "migrationStart" "A migration of a knowledge model editor to a newer version of its parent package was started." "The migrated editor." exampleMigrationData

knowledgeModelEditorsMigrationFinish :: AuditLogEventDefinition () AuditLogKnowledgeModelMigrationData
knowledgeModelEditorsMigrationFinish =
  knowledgeModelEditorEvent
    "migrationFinish"
    "A migration of a knowledge model editor was finished by publishing it; the publish is a knowledgeModels.publish event."
    "The migrated editor."
    exampleMigrationData

knowledgeModelEditorsMigrationCancel :: AuditLogEventDefinition () AuditLogKnowledgeModelMigrationData
knowledgeModelEditorsMigrationCancel =
  knowledgeModelEditorEvent "migrationCancel" "A migration of a knowledge model editor was cancelled." "The editor whose migration was cancelled." exampleMigrationData

knowledgeModelEditorEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
knowledgeModelEditorEvent action description =
  auditLogEvent "knowledgeModelEditors" action description WorkspaceAuditLogScope True KnowledgeModelEditorAuditLogEntityType

exampleKnowledgeModelEditorData :: AuditLogKnowledgeModelEditorData
exampleKnowledgeModelEditorData = AuditLogKnowledgeModelEditorData {id = "myorg:genomics", version = "1.1.0", basedOn = Just exampleKnowledgeModel}

exampleEditorChanges :: AuditLogEditorChanges
exampleEditorChanges =
  AuditLogEditorChanges
    { name = change "Genomics" "Marine Genomics"
    , id = Nothing
    , version = change "1.1.0" "1.2.0"
    , description = Nothing
    , license = Nothing
    , language = Nothing
    }

exampleMigrationData :: AuditLogKnowledgeModelMigrationData
exampleMigrationData =
  AuditLogKnowledgeModelMigrationData
    { sourcePackage = exampleKnowledgeModel
    , targetPackage = AuditLogKnowledgeModelRef {uuid = u' "8b4e6c2a-1d3f-4a5b-9c7e-0f2a4b6c8d35", id = "myorg:genomics", version = "2.0.0", name = "Genomics"}
    }

-- ---------------------------------------------------------------------------------------------------------------------
knowledgeModelSecretsCreate :: AuditLogEventDefinition () NoData
knowledgeModelSecretsCreate = knowledgeModelSecretEvent "create" "A knowledge model secret was created. The value is never recorded." "The created secret." NoData

knowledgeModelSecretsUpdate :: AuditLogEventDefinition AuditLogKnowledgeModelSecretChanges AuditLogKnowledgeModelSecretUpdateData
knowledgeModelSecretsUpdate =
  withChangesExample AuditLogKnowledgeModelSecretChanges {name = change "api-token" "registry-token"} $
    knowledgeModelSecretEvent
      "update"
      "A knowledge model secret changed. A changed name is in changes; the value is never recorded, valueChanged only says whether it changed."
      "The changed secret; entity.name is the name after the change."
      AuditLogKnowledgeModelSecretUpdateData {valueChanged = True}

knowledgeModelSecretsDelete :: AuditLogEventDefinition () NoData
knowledgeModelSecretsDelete = knowledgeModelSecretEvent "delete" "A knowledge model secret was deleted." "The deleted secret." NoData

knowledgeModelSecretEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
knowledgeModelSecretEvent action description =
  auditLogEvent "knowledgeModelSecrets" action description OrganizationOrWorkspaceAuditLogScope True KnowledgeModelSecretAuditLogEntityType

-- ---------------------------------------------------------------------------------------------------------------------
documentTemplatesImportFromFile :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplatesImportFromFile =
  documentTemplateEvent "importFromFile" "A document template bundle was imported from a file." True "The imported template." exampleDocumentTemplateData

documentTemplatesPullFromRegistry :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplatesPullFromRegistry =
  documentTemplateEvent "pullFromRegistry" "A document template bundle was pulled from the registry." True "The pulled template." exampleDocumentTemplateData

documentTemplatesExport :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplatesExport =
  documentTemplateEvent "export" "A document template bundle was exported to a file." False "The exported template." exampleDocumentTemplateData

documentTemplatesPublish :: AuditLogEventDefinition () AuditLogLibraryPublishData
documentTemplatesPublish =
  documentTemplateEvent
    "publish"
    "A document template editor was published; the draft became the released template with the same uuid."
    True
    "The published template."
    AuditLogLibraryPublishData {id = exampleDocumentTemplateData.id, version = exampleDocumentTemplateData.version, editor = AuditLogRef {uuid = u' "3e5a7c9b-2d4f-4b6a-8e1c-5f7a9b1d3e46", name = "Questionnaire Report"}}

documentTemplatesDeprecate :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplatesDeprecate = documentTemplateEvent "deprecate" "A document template was deprecated." True "The deprecated template." exampleDocumentTemplateData

documentTemplatesRestore :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplatesRestore = documentTemplateEvent "restore" "A deprecated document template was released again." True "The restored template." exampleDocumentTemplateData

documentTemplatesDelete :: AuditLogEventDefinition () AuditLogLibraryDeleteData
documentTemplatesDelete =
  documentTemplateEvent
    "delete"
    "A document template was deleted, one version or all versions; dependents counts what depended on the deleted versions."
    True
    "The template the delete was requested for."
    AuditLogLibraryDeleteData {allVersions = False, deletedPackages = ["myorg:questionnaire-report:2.0.0"], dependents = AuditLogLibraryDependentsData {editors = 0, projects = 2, documents = 5}}

documentTemplateEvent :: String -> String -> Bool -> String -> d -> AuditLogEventDefinition c d
documentTemplateEvent action description eventBridge =
  auditLogEvent "documentTemplates" action description OrganizationOrWorkspaceAuditLogScope eventBridge DocumentTemplateAuditLogEntityType

exampleDocumentTemplateData :: AuditLogLibraryData
exampleDocumentTemplateData = AuditLogLibraryData {id = "myorg:questionnaire-report", version = "2.0.0"}

-- ---------------------------------------------------------------------------------------------------------------------
documentTemplateEditorsCreate :: AuditLogEventDefinition () AuditLogDocumentTemplateEditorData
documentTemplateEditorsCreate =
  documentTemplateEditorEvent "create" "A document template editor was created, empty or based on a template." "The created editor (draft template)." exampleDocumentTemplateEditorData

documentTemplateEditorsUpdate :: AuditLogEventDefinition AuditLogEditorChanges NoData
documentTemplateEditorsUpdate =
  withChangesExample exampleEditorChanges $
    documentTemplateEditorEvent
      "update"
      "The details of a document template editor changed. The changed fields are in changes; content edits are not recorded."
      "The changed editor; entity.name is the name after the change."
      NoData

documentTemplateEditorsDelete :: AuditLogEventDefinition () AuditLogLibraryData
documentTemplateEditorsDelete =
  documentTemplateEditorEvent "delete" "A document template editor was deleted." "The deleted editor (draft template)." AuditLogLibraryData {id = "myorg:questionnaire-report", version = "2.1.0"}

documentTemplateEditorEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
documentTemplateEditorEvent action description =
  auditLogEvent "documentTemplateEditors" action description OrganizationOrWorkspaceAuditLogScope True DocumentTemplateEditorAuditLogEntityType

exampleDocumentTemplateEditorData :: AuditLogDocumentTemplateEditorData
exampleDocumentTemplateEditorData =
  AuditLogDocumentTemplateEditorData
    { id = "myorg:questionnaire-report"
    , version = "2.1.0"
    , basedOn = Just AuditLogDocumentTemplateRef {uuid = u' "4f6b8d0a-2c4e-4a6b-9d1f-3a5c7e9b1d57", id = "myorg:questionnaire-report", version = "2.0.0", name = "Questionnaire Report"}
    }

-- ---------------------------------------------------------------------------------------------------------------------
projectsCreate :: AuditLogEventDefinition () AuditLogProjectCreateData
projectsCreate =
  projectEvent
    "create"
    "A project was created in a workspace, from a knowledge model or from a project template."
    "The created project."
    AuditLogProjectCreateData {knowledgeModel = exampleKnowledgeModel, projectTemplate = Nothing, visibility = PrivateProjectVisibility, sharing = RestrictedProjectSharing}

projectsClone :: AuditLogEventDefinition () AuditLogProjectCloneData
projectsClone = projectEvent "clone" "A project was copied together with its content, versions and files." "The new copy." AuditLogProjectCloneData {sourceProject = exampleProject}

projectsImport :: AuditLogEventDefinition () AuditLogProjectImportData
projectsImport =
  projectEvent
    "import"
    "A project was created by an import of projects for a list of users; the users became its owners."
    "The imported project."
    AuditLogProjectImportData {knowledgeModel = exampleKnowledgeModel, owners = [exampleUser]}

projectsUpdate :: AuditLogEventDefinition AuditLogProjectChanges NoData
projectsUpdate =
  withChangesExample exampleProjectChanges $
    projectEvent
      "update"
      "Project settings or sharing changed. The changed fields are in changes; a save that changes nothing writes no row."
      "The changed project; entity.name is the name after the change."
      NoData

projectsPermissionsUpdate :: AuditLogEventDefinition () AuditLogProjectPermissionsData
projectsPermissionsUpdate =
  projectEvent
    "permissionsUpdate"
    "Users or user groups were added to or removed from a project; a changed level appears as a removal of the old and an addition of the new permissions."
    "The project whose permissions changed."
    AuditLogProjectPermissionsData
      { added = [AuditLogProjectMember {memberType = UserProjectPermType, member = AuditLogRef {uuid = exampleUser.uuid, name = exampleUser.email}, perms = ownerPermissions}]
      , removed = []
      }

projectsVersionCreate :: AuditLogEventDefinition () AuditLogProjectVersionData
projectsVersionCreate = projectEvent "versionCreate" "A named version of a project was created." "The project of the version." exampleProjectVersionData

projectsVersionUpdate :: AuditLogEventDefinition AuditLogProjectVersionChanges AuditLogProjectVersionData
projectsVersionUpdate =
  withChangesExample AuditLogProjectVersionChanges {name = change "Draft" "Submitted", description = Nothing} $
    projectEvent "versionUpdate" "A named version of a project was renamed or got another description. The changed fields are in changes." "The project of the version." exampleProjectVersionData

projectsVersionDelete :: AuditLogEventDefinition () AuditLogProjectVersionData
projectsVersionDelete = projectEvent "versionDelete" "A named version of a project was deleted." "The project of the version." exampleProjectVersionData

projectsRevert :: AuditLogEventDefinition () AuditLogProjectRevertData
projectsRevert =
  projectEvent
    "revert"
    "A project was reverted to an earlier event; the later events and versions were deleted."
    "The reverted project."
    AuditLogProjectRevertData {eventUuid = u' "1f3d5b7a-9c2e-4a6b-8d0f-2e4a6c8b0d13", eventCreatedAt = dt' 2026 11 2}

projectsMigrate :: AuditLogEventDefinition () AuditLogProjectMigrateData
projectsMigrate =
  projectEvent
    "migrate"
    "A project was migrated to another version of its knowledge model."
    "The migrated project."
    AuditLogProjectMigrateData {sourceKnowledgeModel = exampleKnowledgeModel, targetKnowledgeModel = exampleMigrationData.targetPackage}

projectsDelete :: AuditLogEventDefinition () NoData
projectsDelete = projectEvent "delete" "A project was deleted, by a user or because nobody had access to it any more." "The deleted project." NoData

projectEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
projectEvent action description = auditLogEvent "projects" action description WorkspaceAuditLogScope True ProjectAuditLogEntityType

exampleProject :: AuditLogRef
exampleProject = AuditLogRef {uuid = u' "9c3a7d41-8e2b-4f6a-b1d0-5e7c2a9f3b64", name = "Marine Genomics DMP"}

exampleProjectVersionData :: AuditLogProjectVersionData
exampleProjectVersionData = AuditLogProjectVersionData {version = AuditLogRef {uuid = u' "6d8f0b2c-4e6a-4c8d-9f1b-3c5e7a9d1f24", name = "Submitted for review"}}

exampleProjectChanges :: AuditLogProjectChanges
exampleProjectChanges =
  AuditLogProjectChanges
    { name = change "Genomics DMP" "Marine Genomics DMP"
    , description = Nothing
    , visibility = change PrivateProjectVisibility VisibleViewProjectVisibility
    , sharing = Nothing
    , projectTags = change [] ["marine"]
    , isTemplate = Nothing
    , documentTemplate = Nothing
    , format = change Nothing (Just AuditLogRef {uuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2", name = "PDF"})
    }

-- ---------------------------------------------------------------------------------------------------------------------
documentsCreate :: AuditLogEventDefinition () AuditLogDocumentCreateData
documentsCreate =
  documentEvent
    "create"
    "A document was generated from a project."
    "The created document."
    AuditLogDocumentCreateData
      { project = exampleProject
      , documentTemplate = AuditLogDocumentTemplateRef {uuid = u' "4f6b8d0a-2c4e-4a6b-9d1f-3a5c7e9b1d57", id = "myorg:questionnaire-report", version = "2.0.0", name = "Questionnaire Report"}
      , format = AuditLogRef {uuid = u' "2a4c6e8b-0d1f-4b3a-8c5e-7a9b1d3f5e68", name = "PDF"}
      }

documentsDelete :: AuditLogEventDefinition () AuditLogDocumentData
documentsDelete = documentEvent "delete" "A document was deleted." "The deleted document." AuditLogDocumentData {project = Just exampleProject}

documentsSubmit :: AuditLogEventDefinition () AuditLogDocumentSubmitData
documentsSubmit =
  documentEvent
    "submit"
    "A document was submitted to a submission service; state is the result of the submission."
    "The submitted document."
    AuditLogDocumentSubmitData
      { project = Just exampleProject
      , service = AuditLogSubmissionServiceRef {id = "data-repository", name = "Data Repository"}
      , state = DoneSubmissionState
      , location = Just "https://repository.example.org/records/123456"
      }

documentEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
documentEvent action description = auditLogEvent "documents" action description WorkspaceAuditLogScope True DocumentAuditLogEntityType

-- ---------------------------------------------------------------------------------------------------------------------
projectFilesUpload :: AuditLogEventDefinition () AuditLogProjectFileData
projectFilesUpload =
  projectFileEvent "upload" "A file was uploaded to a project as a reply." "The uploaded file; entity.name is the file name." AuditLogProjectFileData {project = exampleProject}

projectFilesDelete :: AuditLogEventDefinition () AuditLogProjectFileData
projectFilesDelete = projectFileEvent "delete" "A file was deleted from a project." "The deleted file; entity.name is the file name." AuditLogProjectFileData {project = exampleProject}

projectFileEvent :: String -> String -> String -> d -> AuditLogEventDefinition c d
projectFileEvent action description = auditLogEvent "projectFiles" action description WorkspaceAuditLogScope True ProjectFileAuditLogEntityType
