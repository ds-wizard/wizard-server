module Shared.Api.Resource.Library.LibraryDependentsJM where

import Data.Aeson

import Shared.Model.Library.LibraryDependents
import Shared.Util.Aeson

instance FromJSON LibraryDependents where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LibraryDependents where
  toJSON = genericToJSON jsonOptions

instance FromJSON LibraryDependentKnowledgeModelPackage where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LibraryDependentKnowledgeModelPackage where
  toJSON = genericToJSON jsonOptions

instance FromJSON LibraryDependentResource where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LibraryDependentResource where
  toJSON = genericToJSON jsonOptions

instance FromJSON LibraryHiddenDependents where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LibraryHiddenDependents where
  toJSON = genericToJSON jsonOptions
