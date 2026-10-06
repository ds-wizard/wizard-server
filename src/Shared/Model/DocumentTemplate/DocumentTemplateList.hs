module Shared.Model.DocumentTemplate.DocumentTemplateList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.SemVer2Tuple
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateList = DocumentTemplateList
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , nonEditable :: Bool
  , language :: String
  , potFileReady :: Bool
  , remoteVersion :: Maybe String
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateList where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version
