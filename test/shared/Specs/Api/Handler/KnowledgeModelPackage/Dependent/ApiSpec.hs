module Specs.Api.Handler.KnowledgeModelPackage.Dependent.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.KnowledgeModelPackage.Dependent.List_GET

dependentAPI requestContext =
  describe "KNOWLEDGE MODEL PACKAGE DEPENDENT API Spec" $ do
    list_GET requestContext
