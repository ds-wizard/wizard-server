module Specs.Service.KnowledgeModel.Package.PackageUtilSpec where

import Test.Hspec

import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

packageUtilSpec =
  describe "Package Utils" $ do
    let pkgCoordinate = Coordinate "org.nl.core-nl" "2.0.0"
    let anyPattern = KnowledgeModelPackagePattern {id = Nothing, minVersion = Nothing, maxVersion = Nothing}
    describe "fitsIntoKMSpec" $ do
      it "No restrictions => Allow anything" $
        fitsIntoKMSpec pkgCoordinate anyPattern `shouldBe` True
      it "Restriction on 'id', same provided 'id' => Allow" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {id = Just "org.nl.core-nl"} :: KnowledgeModelPackagePattern) `shouldBe` True
      it "Restriction on 'id', different provided 'id' => Deny" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {id = Just "org.de.core-de"} :: KnowledgeModelPackagePattern) `shouldBe` False
      it "Restriction on 'id', provided 'id' only shares a prefix => Deny" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {id = Just "org.nl"} :: KnowledgeModelPackagePattern) `shouldBe` False
      it "No 'id', restriction on 'minimal version', provided higher version => Allow" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {minVersion = Just "1.0.0"} :: KnowledgeModelPackagePattern) `shouldBe` True
      it "No 'id', restriction on 'minimal version', provided lower version => Deny" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {minVersion = Just "2.0.1"} :: KnowledgeModelPackagePattern) `shouldBe` False
      it "No 'id', restriction on 'maximal version', provided lower version => Allow" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {maxVersion = Just "2.0.1"} :: KnowledgeModelPackagePattern) `shouldBe` True
      it "No 'id', restriction on 'maximal version', provided higher version => Deny" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {maxVersion = Just "1.0.0"} :: KnowledgeModelPackagePattern) `shouldBe` False
      it "Two restrictions, provided data satisfies just one => Deny" $
        fitsIntoKMSpec pkgCoordinate (anyPattern {id = Just "org.nl.core-nl", maxVersion = Just "1.0.0"} :: KnowledgeModelPackagePattern) `shouldBe` False
