module Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl where

import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Service.KnowledgeModel.Editor.EditorAcl

checkViewPermission :: WizardRequestContextC s m => U.UUID -> m ()
checkViewPermission = checkPermissionToEditor

checkEditPermission :: WizardRequestContextC s m => U.UUID -> m ()
checkEditPermission = checkPermissionToEditor
