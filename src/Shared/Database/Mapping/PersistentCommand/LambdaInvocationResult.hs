module Shared.Database.Mapping.PersistentCommand.LambdaInvocationResult where

import Database.PostgreSQL.Simple

import Shared.Model.PersistentCommand.LambdaInvocationResult

instance FromRow LambdaInvocationResult
