module Shared.Model.Project.Comment.ProjectCommentThreadNotification where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.User.UserSimple

data ProjectCommentThreadNotification = ProjectCommentThreadNotification
  { projectUuid :: U.UUID
  , projectName :: String
  , knowledgeModelPackageUuid :: U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , tenantUuid :: U.UUID
  , commentThreadUuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , private :: Bool
  , assignedTo :: UserSimple
  , assignedBy :: Maybe UserSimple
  , text :: String
  , questionTitle :: Maybe String
  , workspaceUuid :: U.UUID
  , appTitle :: Maybe String
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , supportEmail :: Maybe String
  , mailConfigUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord ProjectCommentThreadNotification where
  compare a b = compare a.projectUuid b.projectUuid
