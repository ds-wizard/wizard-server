module Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.User.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

checkViewPermissionToKnowledgeModelPackage :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkViewPermissionToKnowledgeModelPackage Nothing = return ()
checkViewPermissionToKnowledgeModelPackage (Just pkgUuid) = do
  pkg <- findPackageByUuid pkgUuid
  mCurrentUser <- asks (.currentUser')
  unless
    (pkg.public || isJust mCurrentUser)
    (throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER)
