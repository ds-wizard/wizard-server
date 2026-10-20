module Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelTreeQuery
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil
import Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeMapper

getEditorKnowledgeModelTree :: WizardRequestContextC s m => U.UUID -> KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO
getEditorKnowledgeModelTree editorUuid query = do
  checkPermissionToEditor editorUuid
  (_, km) <- findEditorKnowledgeModel editorUuid query.tagUuids
  toTree km query

getPackageKnowledgeModelTree :: WizardRequestContextC s m => U.UUID -> KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO
getPackageKnowledgeModelTree pkgUuid query = do
  checkViewPermissionToKnowledgeModelPackage (Just pkgUuid)
  km <- compileKnowledgeModel [] (Just pkgUuid) query.tagUuids
  toTree km query

toTree :: WizardRequestContextC s m => KnowledgeModel -> KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO
toTree km query = do
  let rootUuid = fromMaybe km.uuid query.entityUuid
  unless
    (entityExists km rootUuid)
    (throwError . NotExistsError $ _ERROR_VALIDATION__KM_ENTITY_ABSENCE (U.toString rootUuid))
  return $ toTreeDTO km rootUuid query
