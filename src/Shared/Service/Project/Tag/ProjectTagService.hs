module Shared.Service.Project.Tag.ProjectTagService where

import Shared.Database.DAO.Project.ProjectTagDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext

getProjectTagSuggestions :: WizardRequestContextC s m => Maybe String -> [String] -> Pageable -> [Sort] -> m (Page String)
getProjectTagSuggestions = findProjectTagsPage
