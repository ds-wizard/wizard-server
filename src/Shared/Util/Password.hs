module Shared.Util.Password where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.PasswordStore as PasswordStore
import qualified Data.ByteString.Char8 as BS

import Shared.Util.String (splitOn)

generatePasswordHash :: MonadIO m => String -> m String
generatePasswordHash password = do
  hash <- liftIO $ BS.unpack <$> PasswordStore.makePasswordWith PasswordStore.pbkdf2 (BS.pack password) 17
  return $ "pbkdf2:" ++ hash

verifyPassword :: String -> String -> Bool
verifyPassword incomingPassword passwordHashFromDB =
  case splitOn ":" passwordHashFromDB of
    ["pbkdf1", hashFromDB] -> PasswordStore.verifyPassword (BS.pack incomingPassword) (BS.pack hashFromDB)
    ["pbkdf2", hashFromDB] ->
      PasswordStore.verifyPasswordWith PasswordStore.pbkdf2 (2 ^) (BS.pack incomingPassword) (BS.pack hashFromDB)
    _ -> False
