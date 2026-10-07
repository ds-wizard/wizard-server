module Shared.Api.Resource.Locale.Common.LocaleUploadSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger.Internal

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem LocaleUploadDTO :> api) where
  toSwagger _ =
    addParam (formParam "file" "PO file to upload" True SwaggerFile)
      . addParam (formParam "confirmed" "Store the file; without it, only a preview is returned" False SwaggerBoolean)
      $ toSwagger (Proxy :: Proxy api)
    where
      formParam name description required paramType =
        Param
          { _paramName = name
          , _paramDescription = Just description
          , _paramRequired = Just required
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just paramType}
                    }
                )
          }
