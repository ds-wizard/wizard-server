module Shared.Model.Context.ContextMappers where

import Data.Pool

import Shared.Model.Context.WizardRequestContext

runWithConnection :: WizardRequestContextC context m => m a -> context -> IO (Either String a)
runWithConnection function context =
  withResource context.dbPool' $ \dbConnection ->
    runRequestContextWithRequestContext function (setDbConnection (Just dbConnection) context)
