module Shared.Model.Worker.CronWorker where

data CronWorker serverContext requestContextM = CronWorker
  { name :: String
  , condition :: serverContext -> Bool
  , cron :: serverContext -> String
  , function :: requestContextM ()
  , wrapInTransaction :: Bool
  }
