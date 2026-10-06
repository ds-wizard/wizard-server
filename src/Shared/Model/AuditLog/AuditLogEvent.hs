module Shared.Model.AuditLog.AuditLogEvent where

import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Proxy (Proxy (..))
import Data.Swagger
import qualified Data.Text as T
import qualified Data.UUID as U
import GHC.Generics

data AuditLogEntity = AuditLogEntity
  { uuid :: U.UUID
  , name :: String
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogEntity

instance ToSchema AuditLogEntity

data Change a = Change
  { old :: a
  , new :: a
  }
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Change a)

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema (Change a) where
  declareNamedSchema _ = changeSchema ["old", "new"] <$> declareSchemaRef (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} ToSchema a => ToSchema (Change (Maybe a)) where
  declareNamedSchema _ = changeSchema [] <$> declareSchemaRef (Proxy :: Proxy a)

changeSchema :: [T.Text] -> Referenced Schema -> NamedSchema
changeSchema required valueSchema =
  NamedSchema Nothing $
    mempty
      { _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
      , _schemaProperties = IOHM.fromList [("old", valueSchema), ("new", valueSchema)]
      , _schemaRequired = required
      }

data NoData = NoData
  deriving (Show, Eq, Generic)

instance ToJSON NoData where
  toJSON NoData = object []

instance ToSchema NoData where
  declareNamedSchema _ =
    return . NamedSchema Nothing $
      mempty {_schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}}

toChangesJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
toChangesJSON = genericToJSON defaultOptions {omitNothingFields = True}

entityOf :: U.UUID -> String -> AuditLogEntity
entityOf entityUuid entityName = AuditLogEntity {uuid = entityUuid, name = entityName, workspaceUuid = Nothing}

entityInWorkspace :: U.UUID -> U.UUID -> String -> AuditLogEntity
entityInWorkspace workspaceUuid entityUuid entityName = AuditLogEntity {uuid = entityUuid, name = entityName, workspaceUuid = Just workspaceUuid}

change :: Eq a => a -> a -> Maybe (Change a)
change oldValue newValue
  | oldValue /= newValue = Just Change {old = oldValue, new = newValue}
  | otherwise = Nothing

mapChange :: (a -> b) -> Change a -> Change b
mapChange f valueChange = Change {old = f valueChange.old, new = f valueChange.new}
