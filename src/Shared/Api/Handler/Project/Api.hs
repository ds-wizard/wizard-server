module Shared.Api.Handler.Project.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Comment.Api
import Shared.Api.Handler.Project.Detail_Content_PUT
import Shared.Api.Handler.Project.Detail_DELETE
import Shared.Api.Handler.Project.Detail_Documents_GET
import Shared.Api.Handler.Project.Detail_Documents_Preview_GET
import Shared.Api.Handler.Project.Detail_GET
import Shared.Api.Handler.Project.Detail_Preview_GET
import Shared.Api.Handler.Project.Detail_Questionnaire_GET
import Shared.Api.Handler.Project.Detail_Report_GET
import Shared.Api.Handler.Project.Detail_Revert_POST
import Shared.Api.Handler.Project.Detail_Revert_Preview_POST
import Shared.Api.Handler.Project.Detail_Settings_GET
import Shared.Api.Handler.Project.Detail_Settings_PUT
import Shared.Api.Handler.Project.Detail_Share_PUT
import Shared.Api.Handler.Project.Detail_WS
import Shared.Api.Handler.Project.Event.Api
import Shared.Api.Handler.Project.File.Api
import Shared.Api.Handler.Project.List_GET
import Shared.Api.Handler.Project.List_POST
import Shared.Api.Handler.Project.List_POST_CloneUuid
import Shared.Api.Handler.Project.List_POST_FromTemplate
import Shared.Api.Handler.Project.Migration.Api
import Shared.Api.Handler.Project.Tag.Api
import Shared.Api.Handler.Project.User.Api
import Shared.Api.Handler.Project.Version.Api
import Shared.Api.Handler.WizardCommon

type ProjectAPI =
  Tags "Project"
    :> ( List_GET
           :<|> List_POST
           :<|> List_POST_FromTemplate
           :<|> List_POST_CloneUuid
           :<|> Detail_GET
           :<|> Detail_Questionnaire_GET
           :<|> Detail_Preview_GET
           :<|> Detail_Settings_GET
           :<|> Detail_Settings_PUT
           :<|> Detail_Share_PUT
           :<|> Detail_DELETE
           :<|> Detail_Content_PUT
           :<|> Detail_Report_GET
           :<|> Detail_Documents_GET
           :<|> Detail_Documents_Preview_GET
           :<|> Detail_WS
           :<|> Detail_Revert_POST
           :<|> Detail_Revert_Preview_POST
           :<|> CommentAPI
           :<|> EventAPI
           :<|> FileAPI
           :<|> MigrationAPI
           :<|> TagAPI
           :<|> UserAPI
           :<|> VersionAPI
       )

projectApi :: Proxy ProjectAPI
projectApi = Proxy

projectServer :: WizardHandlerC s sm r rm => ServerT ProjectAPI sm
projectServer =
  list_GET
    :<|> list_POST
    :<|> list_POST_FromTemplate
    :<|> list_POST_CloneUuid
    :<|> detail_GET
    :<|> detail_questionnaire_GET
    :<|> detail_preview_GET
    :<|> detail_settings_GET
    :<|> detail_settings_PUT
    :<|> detail_share_PUT
    :<|> detail_DELETE
    :<|> detail_content_PUT
    :<|> detail_report_GET
    :<|> detail_documents_GET
    :<|> detail_documents_preview_GET
    :<|> detail_WS
    :<|> detail_revert_POST
    :<|> detail_revert_preview_POST
    :<|> commentServer
    :<|> eventServer
    :<|> fileServer
    :<|> migrationServer
    :<|> tagServer
    :<|> userServer
    :<|> versionServer
