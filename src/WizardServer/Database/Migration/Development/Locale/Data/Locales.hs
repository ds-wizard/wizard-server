module WizardServer.Database.Migration.Development.Locale.Data.Locales where

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Model.Locale.Locale
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleDetailDTO
import WizardServer.Model.Locale.LocaleList
import WizardServer.Service.Locale.LocaleMapper

localeListDefaultEn :: LocaleList
localeListDefaultEn = toLocaleList localeDefaultEn

localeDefaultEnDto :: LocaleDTO
localeDefaultEnDto = toDTO False localeListDefaultEn

localeListNl :: LocaleList
localeListNl = toLocaleList localeNl

localeNlDto :: LocaleDTO
localeNlDto = (toDTO False localeListNl) {organization = Just orgGlobalSimple}

localeNlDetailDto :: LocaleDetailDTO
localeNlDetailDto = toDetailDTO localeNl True [] [globalRegistryOrganization] [(localeNl.uuid, localeNl.version)] Nothing

localeNlChangeDto :: LocaleChangeDTO
localeNlChangeDto =
  LocaleChangeDTO
    { enabled = False
    , defaultLocale = False
    }

localeListDe :: LocaleList
localeListDe = toLocaleList localeDe

localeDeDto :: LocaleDTO
localeDeDto = (toDTO False localeListDe) {organization = Just orgGlobalSimple}
