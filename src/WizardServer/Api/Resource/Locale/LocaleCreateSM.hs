module WizardServer.Api.Resource.Locale.LocaleCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Shared.Api.Resource.Locale.LocaleCreateDTO (LocaleCreateDTO)

instance HasSwagger api => HasSwagger (MultipartForm Mem LocaleCreateDTO :> api) where
  toSwagger _ =
    addParam nameField
      . addParam descriptionField
      . addParam codeField
      . addParam idField
      . addParam versionField
      . addParam licenseField
      . addParam readmeField
      . addParam recommendedAppVersionField
      . addParam wizardContentField
      . addParam mailContentField
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
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      descriptionField =
        Param
          { _paramName = "description"
          , _paramDescription = Just "Description"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      codeField =
        Param
          { _paramName = "code"
          , _paramDescription = Just "Code"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      idField =
        Param
          { _paramName = "id"
          , _paramDescription = Just "ID"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      versionField =
        Param
          { _paramName = "version"
          , _paramDescription = Just "Version"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      licenseField =
        Param
          { _paramName = "license"
          , _paramDescription = Just "License"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      readmeField =
        Param
          { _paramName = "readme"
          , _paramDescription = Just "Readme"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      recommendedAppVersionField =
        Param
          { _paramName = "recommendedAppVersion"
          , _paramDescription = Just "Recommended App Version"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerString}
                    }
                )
          }
      wizardContentField =
        Param
          { _paramName = "wizardContent"
          , _paramDescription = Just "JSON translation file for Data Management Planner"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
      mailContentField =
        Param
          { _paramName = "mailContent"
          , _paramDescription = Just "PO translation file for mails"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
