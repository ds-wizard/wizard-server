module Shared.Model.PersistentCommand.DocumentTemplate.DocumentTemplateGeneratePotFileCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data DocumentTemplateGeneratePotFileCommand = DocumentTemplateGeneratePotFileCommand
  { documentTemplateUuid :: U.UUID
  , organizationId :: String
  , templateId :: String
  , version :: String
  , language :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON DocumentTemplateGeneratePotFileCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateGeneratePotFileCommand where
  toJSON = genericToJSON jsonOptions
