module Specs.Api.Resource.Coordinate.CoordinateJMSpec where

import Data.Aeson
import Data.Aeson.QQ
import Test.Hspec

import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Api.Resource.LocaleBundle.LocaleBundleJM ()
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

coordinateJMSpec =
  describe "Old bundle shape" $ do
    it "knowledge model bundle joins organizationId and kmId into id" $ do
      let result = fromJSON oldKmBundle :: Result KnowledgeModelBundle
      fmap (.id) result `shouldBe` Success "org.nl.core-nl"
      let pkg = fmap (head . (.packages)) result
      fmap (.id) pkg `shouldBe` Success "org.nl.core-nl"
      fmap (fmap show . (.previousPackageId)) pkg `shouldBe` Success (Just "global.core:1.0.0")
      fmap (fmap show . (.forkOfPackageId)) pkg `shouldBe` Success (Just "global.core:1.0.0")
    it "knowledge model bundle in the new shape keeps id and split references" $ do
      let result = fromJSON newKmBundle :: Result KnowledgeModelBundle
      fmap (.id) result `shouldBe` Success "root"
      fmap (fmap show . (.mergeCheckpointPackageId) . head . (.packages)) result `shouldBe` Success (Just "dsw.root:2.0.0")
    it "document template bundle joins organizationId and templateId and rewrites allowedPackages" $ do
      let result = fromJSON oldTemplateBundle :: Result DocumentTemplateBundleDTO
      fmap (.id) result `shouldBe` Success "global.project-report"
      fmap (fmap (.id) . (.allowedPackages)) result `shouldBe` Success [Just "global.core", Nothing]
    it "locale bundle joins organizationId and localeId into id" $ do
      let result = fromJSON oldLocaleBundle :: Result LocaleBundleDTO
      fmap (.id) result `shouldBe` Success "global.dutch"
    it "package pattern with only orgId means any knowledge model" $ do
      let result = fromJSON [aesonQQ|{"orgId": "global", "kmId": null, "minVersion": "1.0.0", "maxVersion": null}|] :: Result KnowledgeModelPackagePattern
      fmap (.id) result `shouldBe` Success Nothing

oldKmBundle :: Value
oldKmBundle =
  [aesonQQ|
    { "id": "org.nl:core-nl:2.0.0", "name": "NL", "organizationId": "org.nl", "kmId": "core-nl", "version": "2.0.0", "metamodelVersion": 17
    , "packages":
        [ { "id": "org.nl:core-nl:2.0.0", "name": "NL", "organizationId": "org.nl", "kmId": "core-nl", "version": "2.0.0", "metamodelVersion": 17
          , "description": "", "previousPackageId": "global:core:1.0.0", "forkOfPackageId": "global:core:1.0.0", "mergeCheckpointPackageId": "global:core:1.0.0", "events": [] }
        ]
    }
  |]

newKmBundle :: Value
newKmBundle =
  [aesonQQ|
    { "id": "root", "name": "Root", "version": "3.0.0", "metamodelVersion": 17
    , "packages":
        [ { "id": "root", "name": "Root", "version": "3.0.0", "metamodelVersion": 17, "description": ""
          , "previousPackageId": null, "previousPackageVersion": null, "forkOfPackageId": "dsw.root", "forkOfPackageVersion": "2.0.0"
          , "mergeCheckpointPackageId": "dsw.root", "mergeCheckpointPackageVersion": "2.0.0", "events": [] }
        ]
    }
  |]

oldTemplateBundle :: Value
oldTemplateBundle =
  [aesonQQ|
    { "id": "global:project-report:1.0.0", "name": "Report", "organizationId": "global", "templateId": "project-report", "version": "1.0.0"
    , "metamodelVersion": "19.0", "description": "", "readme": "", "license": ""
    , "allowedPackages": [{"orgId": "global", "kmId": "core", "minVersion": null, "maxVersion": null}, {"orgId": null, "kmId": null, "minVersion": null, "maxVersion": null}]
    , "formats": [], "files": [], "assets": [], "createdAt": "2018-01-21T00:00:00Z" }
  |]

oldLocaleBundle :: Value
oldLocaleBundle =
  [aesonQQ|
    { "id": "global:dutch:1.0.0", "name": "Dutch", "description": "", "code": "nl", "organizationId": "global", "localeId": "dutch", "version": "1.0.0"
    , "license": "", "readme": "", "recommendedAppVersion": "4.0.0", "createdAt": "2018-01-21T00:00:00Z" }
  |]
