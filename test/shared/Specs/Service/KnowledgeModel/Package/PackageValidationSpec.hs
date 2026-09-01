module Specs.Service.KnowledgeModel.Package.PackageValidationSpec where

import Test.Hspec

import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation
import Specs.Common

packageValidationSpec requestContext =
  describe "Package Validation" $
    it "validateIsVersionHigher" $ do
      shouldSucceed requestContext (validateIsVersionHigher "0.0.1" "0.0.0")
      shouldSucceed requestContext (validateIsVersionHigher "0.1.0" "0.0.0")
      shouldSucceed requestContext (validateIsVersionHigher "0.1.1" "0.0.0")
      shouldSucceed requestContext (validateIsVersionHigher "1.0.0" "0.0.0")
      shouldSucceed requestContext (validateIsVersionHigher "1.2.4" "1.2.3")
      shouldFailed requestContext (validateIsVersionHigher "0.0.0" "0.0.0")
      shouldFailed requestContext (validateIsVersionHigher "1.0.0" "1.0.0")
      shouldFailed requestContext (validateIsVersionHigher "0.1.0" "1.0.0")
      shouldFailed requestContext (validateIsVersionHigher "0.0.1" "1.0.0")
