module Shared.Service.Library.LibraryDependentMapper where

import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)

import Shared.Model.Library.LibraryDependents

toLibraryDependents :: [LibraryDependent] -> LibraryDependents
toLibraryDependents dependents =
  LibraryDependents
    { knowledgeModelPackages = fmap toDependentKnowledgeModelPackage (visibleOf PackageLibraryDependentEntity)
    , editors = fmap toDependentResource (visibleOf EditorLibraryDependentEntity)
    , projects = fmap toDependentResource (visibleOf ProjectLibraryDependentEntity)
    , documents = fmap toDependentResource (visibleOf DocumentLibraryDependentEntity)
    , hidden =
        LibraryHiddenDependents
          { knowledgeModelPackages = countHidden PackageLibraryDependentEntity
          , editors = countHidden EditorLibraryDependentEntity
          , projects = countHidden ProjectLibraryDependentEntity
          , documents = countHidden DocumentLibraryDependentEntity
          , workspaces = length . L.nub . mapMaybe (.workspaceUuid) $ hiddenDependents
          }
    , deleteAllowed = null hiddenDependents
    }
  where
    hiddenDependents = filter (not . (.visible)) dependents
    visibleOf entity = filter (\d -> d.visible && d.entity == entity) dependents
    countHidden entity = length . filter ((== entity) . (.entity)) $ hiddenDependents

toDependentKnowledgeModelPackage :: LibraryDependent -> LibraryDependentKnowledgeModelPackage
toDependentKnowledgeModelPackage dependent =
  LibraryDependentKnowledgeModelPackage
    { uuid = dependent.uuid
    , pId = fromMaybe "" dependent.pId
    , name = dependent.name
    , version = fromMaybe "" dependent.version
    }

toDependentResource :: LibraryDependent -> LibraryDependentResource
toDependentResource dependent = LibraryDependentResource {uuid = dependent.uuid, name = dependent.name}
