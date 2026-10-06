module Specs.Service.Coordinate.CoordinateValidationSpec where

import Data.Maybe (isJust, isNothing)
import Test.Hspec

import Shared.Service.Coordinate.CoordinateValidation

coordinateValidationSpec =
  describe "Coordinate Validation" $ do
    it "isValidVersionFormat" $ do
      isNothing (isValidVersionFormat False "0.0.0") `shouldBe` True
      isNothing (isValidVersionFormat False "1.2.0") `shouldBe` True
      isNothing (isValidVersionFormat False "10.10.10") `shouldBe` True
      isNothing (isValidVersionFormat True "100.100.100") `shouldBe` True
      isNothing (isValidVersionFormat False "100.100.100") `shouldBe` True
      isNothing (isValidVersionFormat True "latest") `shouldBe` True
      isJust (isValidVersionFormat False "latest") `shouldBe` True
      isJust (isValidVersionFormat False "1") `shouldBe` True
      isJust (isValidVersionFormat False "1.") `shouldBe` True
      isJust (isValidVersionFormat False "1.2") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.a") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a.2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a.3.4") `shouldBe` True
    it "isValidIdentifierFormat" $ do
      isNothing (isValidIdentifierFormat "id" "root") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "ab") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "core-nl") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "dsw.root") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "org.nl.core-nl-amsterdam") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "a") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "core_nl") `shouldBe` True
      isNothing (isValidIdentifierFormat "id" "Core.NL.2") `shouldBe` True
      isJust (isValidIdentifierFormat "id" "core:nl") `shouldBe` True
      isJust (isValidIdentifierFormat "id" "code$") `shouldBe` True
      isJust (isValidIdentifierFormat "id" "core nl") `shouldBe` True
      isJust (isValidIdentifierFormat "id" "~.default") `shouldBe` True
      isJust (isValidIdentifierFormat "id" "") `shouldBe` True
