module Specs.Api.Handler.ApiSpec where

import qualified Data.HashMap.Strict.InsOrd as HM
import Data.Swagger (Swagger (..))
import Test.Hspec

import WizardServer.Api.Handler.Swagger.Api

declaredPaths :: [FilePath]
declaredPaths = HM.keys (swagger "0.0.0")._swaggerPaths

shouldServe :: [FilePath] -> Expectation
shouldServe = mapM_ (\path -> declaredPaths `shouldContain` [path])

apiSpec =
  describe "Wizard API" $ do
    it "serves the feature endpoints flat" $
      shouldServe ["/projects", "/knowledge-model-packages", "/document-templates", "/type-hints", "/workspaces", "/workspaces/{uuid}/members/{userUuid}"]
    it "serves the management endpoints flat" $
      shouldServe ["/users", "/users/current/submission-props", "/user-groups/suggestions", "/tokens/system"]
    it "serves one bootstrap and the settings flat" $
      shouldServe ["/bootstrap", "/bootstrap/workspace", "/settings/support", "/settings/registry"]
    it "mounts neither the old config endpoints nor the fair-wizard ones" $
      filter (`elem` ["/configs/bootstrap", "/tenants/current/config", "/settings/analytics", "/settings/look-and-feel/logo", "/registry/signup", "/registry/confirmation", "/workspaces/{uuid}/logo"]) declaredPaths `shouldBe` []
