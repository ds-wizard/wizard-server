module Shared.Service.Owl.OwlService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.KnowledgeModel.Compiler.Compiler
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Service.Owl.Convertor.OwlConvertor
import Shared.Service.Owl.Diff.Differ
import Shared.Service.Owl.OwlMapper
import Shared.Util.Logger
import Shared.Util.Uuid

importOwl :: WizardRequestContextC s m => KnowledgeModelBundleFile -> m KnowledgeModelPackageSimpleDTO
importOwl reqDto = do
  logInfoI _CMP_SERVICE "Importing OWL..."
  case (reqDto.rootElement, reqDto.name, reqDto.organizationId, reqDto.kmId, reqDto.version) of
    (Just rootElement, Just name, Just organizationId, Just kmId, Just version) -> do
      tenantUuid <- asks (.tenantUuid')
      uuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      previousPackageUuid <-
        case reqDto.previousPackageId of
          Just previousPackageId -> do
            pkg <- findPackageByCoordinate previousPackageId
            return . Just $ pkg.uuid
          Nothing -> return Nothing
      events <- importEvents previousPackageUuid rootElement (TE.decodeUtf8 . BSL.toStrict $ reqDto.content)
      let pkg = fromOwl uuid name organizationId kmId version previousPackageUuid tenantUuid now
      let pkgEvents = fmap (toPackageEvent pkg.uuid tenantUuid) events
      insertPackage pkg
      traverse_ insertPackageEvent pkgEvents
      return $ toSimpleDTO pkg
    _ -> throwError . UserError $ _ERROR_VALIDATION__FIELDS_ABSENCE

importEvents :: WizardRequestContextC s m => Maybe U.UUID -> T.Text -> T.Text -> m [KnowledgeModelEvent]
importEvents mPreviousPackageUuid rootElement content = do
  pkgEvents <- convertOwlToEvents rootElement content
  case mPreviousPackageUuid of
    Just previousPackageUuid -> do
      previousPackageEvents <- findPackageEvents previousPackageUuid
      let (Right km1) = compile Nothing pkgEvents
      let (Right km2) = compile Nothing (fmap toEvent previousPackageEvents)
      diffKnowledgeModel (km1, km2)
    Nothing -> return pkgEvents

modifyOwlFeature :: WizardRequestContextC s m => Bool -> m ()
modifyOwlFeature owlEnabled =
  runInTransaction $ do
    tcOwl <- findTenantConfigOwl
    let tcOwlUpdated = tcOwl {enabled = owlEnabled} :: TenantConfigOwl
    updateTenantConfigOwl tcOwlUpdated
    return ()

setOwlProperties :: WizardRequestContextC s m => String -> String -> String -> String -> Maybe String -> String -> m ()
setOwlProperties name organizationId kmId version previousKnowledgeModelPackageId rootElement =
  runInTransaction $ do
    tcOwl <- findTenantConfigOwl
    let tcOwlUpdated =
          tcOwl
            { name = name
            , organizationId = organizationId
            , kmId = kmId
            , version = version
            , previousKnowledgeModelPackageId = previousKnowledgeModelPackageId
            , rootElement = rootElement
            }
            :: TenantConfigOwl
    updateTenantConfigOwl tcOwlUpdated
    return ()
