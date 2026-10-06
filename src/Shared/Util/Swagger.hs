module Shared.Util.Swagger where

import Data.Aeson
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.HashSet.InsOrd as InsOrdHS
import Data.List (nub, sortOn)
import Data.Swagger
import qualified Data.Text as T

import Shared.Util.Aeson

toSwagger exampleDTO proxy =
  let schema = genericDeclareNamedSchema (fromAesonOptions jsonOptions) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

toSwaggerWithType typeFieldName exampleDTO proxy =
  let schema = genericDeclareNamedSchema (fromAesonOptions (jsonOptionsWithTypeField typeFieldName)) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

toSwaggerWithFlatType typeFieldName exampleDTO proxy =
  let schema = genericDeclareNamedSchemaUnrestricted (fromAesonOptions (jsonOptionsWithTypeField typeFieldName)) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

withSchemaName dtoName = fmap (\s -> s {_namedSchemaName = Just dtoName})

toSwaggerWithDtoName dtoName exampleDTO proxy =
  let schema = genericDeclareNamedSchema ((fromAesonOptions jsonOptions) {fieldLabelModifier = changePageFields}) proxy
   in withSchemaName dtoName . fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) $ schema

changePageFields :: String -> String
changePageFields "name" = "name"
changePageFields "metadata" = "page"
changePageFields "entities" = "_embedded"
changePageFields field = field

normalizeSwagger :: Swagger -> Swagger
normalizeSwagger = sortTags . runIdentity . allOperations (Identity . updateOperation)
  where
    updateOperation operation =
      let params = fmap updateParam <$> operation._operationParameters
       in operation {_operationParameters = params, _operationConsumes = updateConsumes params operation._operationConsumes}
    updateConsumes params consumes
      | any isFormData params = Just (MimeList ["multipart/form-data"])
      | otherwise = consumes
    isFormData (Inline Param {_paramSchema = ParamOther other}) = other._paramOtherSchemaIn == ParamFormData
    isFormData _ = False
    updateParam param =
      case param._paramSchema of
        ParamOther other -> param {_paramSchema = ParamOther other {_paramOtherSchemaParamSchema = updateItems other._paramOtherSchemaParamSchema}}
        _ -> param
    updateItems paramSchema =
      case paramSchema._paramSchemaItems of
        Just (SwaggerItemsPrimitive Nothing items) -> paramSchema {_paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionCSV) items)}
        _ -> paramSchema

sortTags :: Swagger -> Swagger
sortTags s =
  let names = getConst (allOperations (Const . InsOrdHS.toList . _operationTags) s)
      tags = fmap (\name -> Tag name Nothing Nothing) . sortOn T.toLower . nub $ names
   in s {_swaggerTags = InsOrdHS.fromList tags}
