module WizardServer.Service.Locale.LocaleUtil where

import qualified Data.List as L

selectLocaleById locale = L.find (\l -> l.id == locale.id)
