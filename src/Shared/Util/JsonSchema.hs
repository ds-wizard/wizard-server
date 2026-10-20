module Shared.Util.JsonSchema where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Kind (Type)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits

class JsonSchema a where
  jsonSchema :: Proxy a -> Value
  listSchema :: Proxy a -> Value
  listSchema = arraySchema . jsonSchema
  mayBeOmitted :: Proxy a -> Bool
  mayBeOmitted _ = False

instance JsonSchema Char where
  jsonSchema _ = stringSchema
  listSchema _ = stringSchema

instance JsonSchema T.Text where
  jsonSchema _ = stringSchema

instance JsonSchema a => JsonSchema [a] where
  jsonSchema _ = listSchema (Proxy :: Proxy a)

instance JsonSchema a => JsonSchema (V.Vector a) where
  jsonSchema _ = listSchema (Proxy :: Proxy a)

instance JsonSchema a => JsonSchema (Maybe a) where
  jsonSchema _ = nullableSchema (jsonSchema (Proxy :: Proxy a))

instance JsonSchema Bool where
  jsonSchema _ = typeSchema "boolean"

instance JsonSchema Int where
  jsonSchema _ = typeSchema "integer"

instance JsonSchema Int64 where
  jsonSchema _ = typeSchema "integer"

instance JsonSchema Integer where
  jsonSchema _ = typeSchema "integer"

instance JsonSchema Double where
  jsonSchema _ = typeSchema "number"

instance JsonSchema U.UUID where
  jsonSchema _ = formatSchema "uuid"

instance JsonSchema UTCTime where
  jsonSchema _ = formatSchema "date-time"

instance JsonSchema Value where
  jsonSchema _ = object []

instance JsonSchema v => JsonSchema (KM.KeyMap v) where
  jsonSchema _ = mapSchema (jsonSchema (Proxy :: Proxy v))

instance JsonSchema v => JsonSchema (M.Map k v) where
  jsonSchema _ = mapSchema (jsonSchema (Proxy :: Proxy v))

-- ---------------------------------------------------------------------------------------------------------------------
typeSchema :: String -> Value
typeSchema name = object ["type" .= name]

stringSchema :: Value
stringSchema = typeSchema "string"

formatSchema :: String -> Value
formatSchema format = object ["type" .= ("string" :: String), "format" .= format]

enumSchema :: [String] -> Value
enumSchema values = object ["type" .= ("string" :: String), "enum" .= values]

constSchema :: ToJSON v => v -> Value
constSchema value = object ["const" .= value]

arraySchema :: Value -> Value
arraySchema items = object ["type" .= ("array" :: String), "items" .= items]

mapSchema :: Value -> Value
mapSchema values = object ["type" .= ("object" :: String), "additionalProperties" .= values]

nullableSchema :: Value -> Value
nullableSchema schema = object ["anyOf" .= [schema, typeSchema "null"]]

oneOfSchema :: [Value] -> Value
oneOfSchema [schema] = schema
oneOfSchema schemas = object ["oneOf" .= schemas]

anyOfSchema :: [Value] -> Value
anyOfSchema schemas = object ["anyOf" .= schemas]

describedSchema :: String -> Value -> Value
describedSchema description (Object schema) = Object $ KM.insert "description" (toJSON description) schema
describedSchema _ schema = schema

withConstProperties :: [(String, String)] -> Value -> Value
withConstProperties constants (Object schema)
  | Just (Array branches) <- KM.lookup "oneOf" schema = Object $ KM.insert "oneOf" (Array (fmap (withConstProperties constants) branches)) schema
withConstProperties constants schema = withRequired (fmap fst constants) . withProperties [(name, constSchema value) | (name, value) <- constants] $ schema

withProperties :: [(String, Value)] -> Value -> Value
withProperties properties (Object schema) =
  let existing = case KM.lookup "properties" schema of
        Just (Object o) -> o
        _ -> KM.empty
   in Object $ KM.insert "properties" (Object $ KM.union (KM.fromList [(K.fromString name, property) | (name, property) <- properties]) existing) schema
withProperties _ schema = schema

withoutProperties :: [String] -> Value -> Value
withoutProperties names (Object schema)
  | Just (Object properties) <- KM.lookup "properties" schema =
      Object $ KM.insert "properties" (Object $ foldr (KM.delete . K.fromString) properties names) schema
withoutProperties _ schema = schema

withRequired :: [String] -> Value -> Value
withRequired names (Object schema) =
  let existing = case KM.lookup "required" schema of
        Just (Array values) -> V.toList values
        _ -> []
   in Object $ KM.insert "required" (toJSON (L.nub (fmap toJSON names ++ existing))) schema
withRequired _ schema = schema

partialSchema :: Value -> Value
partialSchema (Object schema) = Object . KM.delete "required" $ KM.mapWithKey partialChild schema
  where
    partialChild "properties" (Object properties) = Object (fmap partialSchema properties)
    partialChild "anyOf" (Array branches) = Array (fmap partialSchema branches)
    partialChild _ value = value
partialSchema schema = schema

definitionSchema :: String -> Value
definitionSchema name = object ["$ref" .= ("#/$defs/" ++ name)]

withDefinitions :: [(String, Value)] -> Value -> Value
withDefinitions definitions (Object schema) = Object $ KM.insert "$defs" (object [K.fromString name .= definition | (name, definition) <- definitions]) schema
withDefinitions _ schema = schema

objectSchema :: [String] -> [(String, Value)] -> Value
objectSchema required properties =
  object $
    [ "type" .= ("object" :: String)
    , "properties" .= object (fmap (\(name, schema) -> K.fromString name .= schema) properties)
    , "additionalProperties" .= False
    ]
      ++ ["required" .= required | not (null required)]

-- ---------------------------------------------------------------------------------------------------------------------
data ConstructorSchema = ConstructorSchema
  { tag :: String
  , fields :: [FieldSchema]
  }

data FieldSchema = FieldSchema
  { name :: String
  , schema :: Value
  , required :: Bool
  }

genericJsonSchema :: forall a. (Generic a, GConstructors (Rep a)) => Options -> Proxy a -> Value
genericJsonSchema options _ = sumSchema options (gConstructors options (Proxy :: Proxy (Rep a)))

sumSchema :: Options -> [ConstructorSchema] -> Value
sumSchema options [constructor] | not (tagSingleConstructors options) = contentSchema constructor
sumSchema options constructors
  | allNullaryToStringTag options && all (null . (.fields)) constructors = enumSchema (fmap (.tag) constructors)
  | otherwise = oneOfSchema (fmap (taggedSchema options) constructors)

contentSchema :: ConstructorSchema -> Value
contentSchema (ConstructorSchema _ [FieldSchema "" schema _]) = schema
contentSchema constructor = fieldsSchema constructor.fields

taggedSchema :: Options -> ConstructorSchema -> Value
taggedSchema options constructor =
  let (tagField, contentsField) = taggedFields options
   in fieldsSchema (FieldSchema tagField (constSchema constructor.tag) True : fmap (namedContents contentsField) constructor.fields)

namedContents :: String -> FieldSchema -> FieldSchema
namedContents contentsField (FieldSchema "" schema required) = FieldSchema contentsField schema required
namedContents _ field = field

taggedFields :: Options -> (String, String)
taggedFields options =
  case sumEncoding options of
    TaggedObject tagField contentsField -> (tagField, contentsField)
    _ -> ("tag", "contents")

fieldsSchema :: [FieldSchema] -> Value
fieldsSchema fields = objectSchema [field.name | field <- fields, field.required] [(field.name, field.schema) | field <- fields]

class GConstructors (f :: Type -> Type) where
  gConstructors :: Options -> Proxy f -> [ConstructorSchema]

instance GConstructors f => GConstructors (D1 d f) where
  gConstructors options _ = gConstructors options (Proxy :: Proxy f)

instance (GConstructors f, GConstructors g) => GConstructors (f :+: g) where
  gConstructors options _ = gConstructors options (Proxy :: Proxy f) ++ gConstructors options (Proxy :: Proxy g)

instance (KnownSymbol name, GFields f) => GConstructors (C1 ('MetaCons name fixity isRecord) f) where
  gConstructors options _ = [ConstructorSchema (constructorTagModifier options (symbolVal (Proxy :: Proxy name))) (gFields options (Proxy :: Proxy f))]

class GFields (f :: Type -> Type) where
  gFields :: Options -> Proxy f -> [FieldSchema]

instance GFields U1 where
  gFields _ _ = []

instance (GFields f, GFields g) => GFields (f :*: g) where
  gFields options _ = gFields options (Proxy :: Proxy f) ++ gFields options (Proxy :: Proxy g)

instance JsonSchema a => GFields (S1 ('MetaSel 'Nothing su ss ds) (K1 i a)) where
  gFields _ _ = [FieldSchema "" (jsonSchema (Proxy :: Proxy a)) True]

instance {-# OVERLAPPABLE #-} (KnownSymbol name, JsonSchema a) => GFields (S1 ('MetaSel ('Just name) su ss ds) (K1 i a)) where
  gFields options _ = [FieldSchema (fieldName options (Proxy :: Proxy name)) (jsonSchema (Proxy :: Proxy a)) (not (mayBeOmitted (Proxy :: Proxy a)))]

instance {-# OVERLAPPING #-} (KnownSymbol name, JsonSchema a) => GFields (S1 ('MetaSel ('Just name) su ss ds) (K1 i (Maybe a))) where
  gFields options _ = [FieldSchema (fieldName options (Proxy :: Proxy name)) (jsonSchema (Proxy :: Proxy (Maybe a))) (not (allowOmittedFields options))]

fieldName :: KnownSymbol name => Options -> Proxy name -> String
fieldName options = fieldLabelModifier options . symbolVal
