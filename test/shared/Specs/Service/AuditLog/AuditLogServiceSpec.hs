module Specs.Service.AuditLog.AuditLogServiceSpec where

import Test.Hspec

import Shared.Database.DAO.AuditLog.AuditLogDAO
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.UserGroupAuditLogData
import Shared.Model.User.UserGroup
import Shared.Service.AuditLog.AuditLogService

import Specs.Common

auditLogServiceSpec requestContext =
  describe "AuditLog Service" $
    it "writes nothing in the wizard-server context" $
      -- GIVEN:
      do
        let entity = entityInWorkspace bioGroup.workspaceUuid bioGroup.uuid bioGroup.name
        let userGroupData = AuditLogUserGroupData {description = bioGroup.description, private = bioGroup.private}
        -- WHEN:
        (Right ()) <- runInContext (logAuditLog userGroupsCreate entity userGroupData) requestContext
        -- THEN:
        (Right auditLogs) <- runInContext findAuditLogs requestContext
        auditLogs `shouldBe` []
