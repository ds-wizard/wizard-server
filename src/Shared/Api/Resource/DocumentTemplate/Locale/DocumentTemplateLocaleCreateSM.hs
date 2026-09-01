module Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO (DocumentTemplateLocaleCreateDTO)

instance HasSwagger api => HasSwagger (MultipartForm Mem DocumentTemplateLocaleCreateDTO :> api) where
  toSwagger _ =
    addParam nameField
      . addParam poContentField
      $ toSwagger (Proxy :: Proxy api)
    where
      nameField =
        Param
          { _paramName = "name"
          , _paramDescription = Just "Name"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      poContentField =
        Param
          { _paramName = "poContent"
          , _paramDescription = Just "PO translation file for the document template"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
