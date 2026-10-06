module Shared.Database.DAO.Settings.SettingsDAO where

import Control.Monad (forM_, void)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.String (f')

data SettingsKey = SettingsKey
  { table :: String
  , constraint :: String
  , columns :: [(String, Action)]
  }

orgKey :: String -> U.UUID -> SettingsKey
orgKey table tenantUuid = SettingsKey table (table ++ "_pk") [("tenant_uuid", toField tenantUuid)]

scopedKey :: String -> U.UUID -> Maybe U.UUID -> SettingsKey
scopedKey table tenantUuid mWorkspaceUuid = SettingsKey table (table ++ "_key") [("tenant_uuid", toField tenantUuid), ("workspace_uuid", toField mWorkspaceUuid)]

childKey :: String -> SettingsKey -> SettingsKey
childKey table key = key {table = table}

keyCondition :: SettingsKey -> String
keyCondition key = L.intercalate " AND " (fmap (columnCondition . fst) key.columns)

columnCondition :: String -> String
columnCondition "workspace_uuid" = "workspace_uuid IS NOT DISTINCT FROM ?"
columnCondition column = column ++ " = ?"

keyParams :: SettingsKey -> [Action]
keyParams key = fmap snd key.columns

findSettingsRow :: (WizardRequestContextC s m, FromRow a) => [String] -> SettingsKey -> m (Maybe a)
findSettingsRow columns key = listToMaybe <$> findSettingsRows columns "" key

findSettingsChildRows :: (WizardRequestContextC s m, FromRow a) => [String] -> SettingsKey -> m [a]
findSettingsChildRows columns = findSettingsRows columns "ORDER BY position"

findSettingsRows :: (WizardRequestContextC s m, FromRow a) => [String] -> String -> SettingsKey -> m [a]
findSettingsRows columns order key = do
  let sql = fromString $ f' "SELECT %s FROM %s WHERE %s %s" [L.intercalate ", " columns, key.table, keyCondition key, order]
  let params = keyParams key
  logQuery sql params
  runDB (\conn -> query conn sql params)

saveSettingsRow :: (WizardRequestContextC s m, ToRow a) => [String] -> SettingsKey -> a -> m ()
saveSettingsRow columns key value = do
  let allColumns = fmap fst key.columns ++ columns ++ ["created_at", "updated_at"]
  let updates = fmap (\column -> column ++ " = EXCLUDED." ++ column) columns ++ ["updated_at = now()"]
  let sql =
        fromString $
          f'
            "INSERT INTO %s (%s) VALUES (%s, now(), now()) ON CONFLICT ON CONSTRAINT %s DO UPDATE SET %s"
            [key.table, L.intercalate ", " allColumns, generateQuestionMarks (fmap fst key.columns ++ columns), key.constraint, L.intercalate ", " updates]
  let params = keyParams key ++ toRow value
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

replaceSettingsChildRows :: (WizardRequestContextC s m, ToRow a) => [String] -> SettingsKey -> [a] -> m ()
replaceSettingsChildRows columns key values = do
  deleteSettingsRowsByKey key
  forM_ (zip [0 :: Int ..] values) $ \(position, value) ->
    insertSettingsRow ("position" : columns) key (toField position : toRow value)

insertSettingsRow :: (WizardRequestContextC s m, ToRow a) => [String] -> SettingsKey -> a -> m ()
insertSettingsRow columns key value = do
  let allColumns = fmap fst key.columns ++ columns
  let sql = fromString $ f' "INSERT INTO %s (%s) VALUES (%s)" [key.table, L.intercalate ", " allColumns, generateQuestionMarks allColumns]
  let params = keyParams key ++ toRow value
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteSettingsRowsByKey :: WizardRequestContextC s m => SettingsKey -> m ()
deleteSettingsRowsByKey key = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s" [key.table, keyCondition key]
  let params = keyParams key
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteSettingsWorkspaceRows :: WizardRequestContextC s m => [String] -> U.UUID -> m ()
deleteSettingsWorkspaceRows tables tenantUuid =
  forM_ tables $ \table -> do
    let sql = fromString $ f' "DELETE FROM %s WHERE tenant_uuid = ? AND workspace_uuid IS NOT NULL" [table]
    let params = [toField tenantUuid]
    logQuery sql params
    runDB (\conn -> execute conn sql params)

findSettingsOverrideAllowed :: WizardRequestContextC s m => String -> U.UUID -> m (Maybe Bool)
findSettingsOverrideAllowed table tenantUuid = do
  let sql = fromString $ f' "SELECT workspace_override_allowed FROM %s WHERE tenant_uuid = ? AND workspace_uuid IS NULL" [table]
  let params = [toField tenantUuid]
  logQuery sql params
  results <- runDB (\conn -> query conn sql params)
  return $ fromOnly <$> listToMaybe results

updateSettingsOverrideAllowed :: WizardRequestContextC s m => String -> U.UUID -> Bool -> m ()
updateSettingsOverrideAllowed table tenantUuid overrideAllowed = do
  let sql = fromString $ f' "UPDATE %s SET workspace_override_allowed = ? WHERE tenant_uuid = ? AND workspace_uuid IS NULL" [table]
  let params = [toField overrideAllowed, toField tenantUuid]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteSettingsTable :: WizardRequestContextC s m => String -> m ()
deleteSettingsTable table = do
  let sql = fromString $ f' "DELETE FROM %s" [table]
  logQuery sql ()
  void $ runDB (`execute_` sql)
