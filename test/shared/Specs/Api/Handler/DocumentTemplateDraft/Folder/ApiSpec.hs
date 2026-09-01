module Specs.Api.Handler.DocumentTemplateDraft.Folder.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST
import Specs.Api.Handler.DocumentTemplateDraft.Folder.List_Move_POST

documentTemplateDraftFolderAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT TEMPLATE DRAFT FOLDER API Spec" $ do
      list_delete_POST requestContext
      list_move_POST requestContext
