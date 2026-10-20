module Specs.Service.Mcp.Tool.LocaleTools.Common where

import Data.Aeson
import qualified Data.List as L
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Locale.LocaleDAO
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as Locale
import Shared.Model.Locale.Locale
import SharedTest.Specs.Api.Mcp

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runLocaleMigrations requestContext = do
  runInContextIO Locale.runMigration requestContext
  runInContextIO Locale.runS3Migration requestContext

localeCodesOf :: Value -> [String]
localeCodesOf structuredContent =
  case valueField "locales" structuredContent of
    Just (Array locales) -> L.sort . concatMap (maybe [] pure . stringField "code") . V.toList $ locales
    _ -> []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertLocaleCodesInDB requestContext expCodes = do
  locales <- getOneFromDB findLocales requestContext
  liftIO $ L.sort (fmap (.code) locales) `shouldBe` L.sort expCodes
