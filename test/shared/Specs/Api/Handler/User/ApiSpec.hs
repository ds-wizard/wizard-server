module Specs.Api.Handler.User.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Detail_DELETE
import Specs.Api.Handler.User.Detail_GET
import Specs.Api.Handler.User.Detail_PUT
import Specs.Api.Handler.User.Detail_Password_Hash_PUT
import Specs.Api.Handler.User.Detail_Password_PUT
import Specs.Api.Handler.User.Detail_State_PUT
import Specs.Api.Handler.User.List_Current_GET
import Specs.Api.Handler.User.List_Current_Locale_GET
import Specs.Api.Handler.User.List_Current_Locale_PUT
import Specs.Api.Handler.User.List_Current_PUT
import Specs.Api.Handler.User.List_Current_Password_PUT
import Specs.Api.Handler.User.List_Current_Submission_Props_GET
import Specs.Api.Handler.User.List_Current_Submission_Props_PUT
import Specs.Api.Handler.User.List_GET
import Specs.Api.Handler.User.List_POST
import Specs.Api.Handler.User.List_Suggestions_GET
import Specs.Api.Handler.User.PluginSettings.ApiSpec

userAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER API Spec" $ do
      list_GET requestContext
      list_suggestions_GET requestContext
      list_POST requestContext
      list_current_GET requestContext
      list_current_PUT requestContext
      list_current_submission_props_GET requestContext
      list_current_submission_props_PUT requestContext
      list_current_password_PUT requestContext
      list_current_locale_GET requestContext
      list_current_locale_PUT requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_password_PUT requestContext
      detail_password_hash_PUT requestContext
      detail_state_PUT requestContext
      userPluginSettingsAPI requestContext
