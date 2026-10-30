module Specs.Service.Document.DocumentServiceSpec where

import Data.Aeson (Value (..), decode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Model.Common.Lens
import Shared.Model.Document.DocumentContext
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Tenant.Limit.TenantLimitBundle
import Shared.Service.Document.Context.DocumentContextService
import Shared.Service.Document.DocumentService
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

documentServiceSpec requestContext =
  describe "DocumentService" $
    describe "createDocument" $
      it "queues the worker command with the document context and the tenant limits" $ do
        runInContextIO USR.runMigration requestContext
        runInContextIO TML.runMigration requestContext
        runInContextIO PRJ.runMigration requestContext
        runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
        runInContextIO deleteDocuments requestContext
        runInContextIO deletePersistentCommands requestContext
        (Right (ctx, _)) <- runInContext (createProjectDocumentContext project1 Nothing) requestContext
        (Right doc) <- runInContext (createDocument (doc1Create {projectEventUuid = Nothing} :: DocumentCreateDTO)) requestContext
        (Right commands) <- runInContext (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
        let [command] = filter (\c -> c.component == "document_worker") commands
        command.function `shouldBe` "generateDocument"
        let body = fromMaybe Null (decode (BSL.pack command.body))
        let document = jsonField "document" body
        jsonString (jsonField "hash" body) `shouldBe` ctx.hash
        document `shouldNotBe` Null
        jsonField "uuid" document `shouldBe` toJSON doc.uuid
        jsonField "preview" document `shouldBe` Bool False
        jsonField "eventUuid" document `shouldBe` toJSON (getUuid . last $ project1Events)
        jsonKeys (jsonField "documentTemplate" document) `shouldBe` L.sort ["uuid", "id", "version", "workspaceUuid"]
        jsonKeys (jsonField "format" document) `shouldBe` L.sort ["uuid", "name"]
        jsonField "createdBy" document `shouldNotBe` Null
        jsonField "storage" (jsonField "limits" body) `shouldBe` toJSON defaultTenantLimitBundle.storage
