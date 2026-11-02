module SharedTest.Specs.PersistentCommand where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext

findLambdaInvoked :: RequestContextC s sc m => U.UUID -> m [Bool]
findLambdaInvoked uuid = fmap fromOnly <$> runDB (\conn -> query conn "SELECT lambda_invoked FROM persistent_command WHERE uuid = ?" (Only uuid))
