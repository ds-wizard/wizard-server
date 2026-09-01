module Shared.Service.Project.User.ProjectUserService where

import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectUserDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Project
import Shared.Model.User.UserSuggestion
import Shared.Service.Project.ProjectAcl
import Shared.Service.User.UserService
import Shared.Service.User.WizardUserMapper

getProjectUserSuggestionsPage :: WizardRequestContextC s m => U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page UserSuggestion)
getProjectUserSuggestionsPage projectUuid mQuery mEditor pageable sort = do
  project <- findProjectByUuid projectUuid
  checkCommentPermissionToProject project.visibility project.sharing project.permissions
  if project.visibility == VisibleCommentProjectVisibility || project.visibility == VisibleEditProjectVisibility || project.sharing == AnyoneWithLinkCommentProjectSharing || project.sharing == AnyoneWithLinkEditProjectSharing
    then getUserSuggestionsPage mQuery Nothing Nothing pageable sort
    else do
      let perm =
            case mEditor of
              Just True -> "EDIT"
              _ -> "COMMENT"
      suggestionPage <- findProjectUserSuggestionsPage projectUuid perm mQuery pageable sort
      return . fmap toSuggestion $ suggestionPage
