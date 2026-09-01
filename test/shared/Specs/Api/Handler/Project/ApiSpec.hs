module Specs.Api.Handler.Project.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Comment.ApiSpec
import Specs.Api.Handler.Project.Detail_Content_PUT
import Specs.Api.Handler.Project.Detail_DELETE
import Specs.Api.Handler.Project.Detail_Documents_GET
import Specs.Api.Handler.Project.Detail_GET
import Specs.Api.Handler.Project.Detail_Preview_GET
import Specs.Api.Handler.Project.Detail_Questionnaire_GET
import Specs.Api.Handler.Project.Detail_Report_GET
import Specs.Api.Handler.Project.Detail_Revert_POST
import Specs.Api.Handler.Project.Detail_Revert_Preview_POST
import Specs.Api.Handler.Project.Detail_Settings_GET
import Specs.Api.Handler.Project.Detail_Settings_PUT
import Specs.Api.Handler.Project.Detail_Share_PUT
import Specs.Api.Handler.Project.Event.ApiSpec
import Specs.Api.Handler.Project.List_GET
import Specs.Api.Handler.Project.List_POST
import Specs.Api.Handler.Project.List_POST_CloneUuid
import Specs.Api.Handler.Project.List_POST_FromTemplate
import Specs.Api.Handler.Project.Migration.ApiSpec
import Specs.Api.Handler.Project.ProjectTag.ApiSpec
import Specs.Api.Handler.Project.User.ApiSpec
import Specs.Api.Handler.Project.Version.ApiSpec

projectAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "PROJECT API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      list_POST_fromTemplate requestContext
      list_POST_cloneUuid requestContext
      detail_GET requestContext
      detail_questionnaire_GET requestContext
      detail_share_PUT requestContext
      detail_preview_GET requestContext
      detail_settings_GET requestContext
      detail_settings_PUT requestContext
      detail_DELETE requestContext
      detail_content_PUT requestContext
      detail_report_GET requestContext
      detail_documents_GET requestContext
      detail_revert_POST requestContext
      detail_revert_preview_POST requestContext
      projectCommentAPI requestContext
      projectEventAPI requestContext
      projectMigrationAPI requestContext
      projectTagAPI requestContext
      projectUserAPI requestContext
      projectVersionAPI requestContext
