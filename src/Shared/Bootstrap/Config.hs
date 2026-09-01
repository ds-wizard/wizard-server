module Shared.Bootstrap.Config where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither')
import System.Exit

import Shared.Model.Error.Error

loadConfig fileName = loadConfigWith fileName fileName

loadConfigWith label source loadFn = do
  eitherConfig <- loadFn source
  case eitherConfig of
    Right config -> do
      print ("Config '" ++ label ++ "' loaded")
      return config
    Left error -> do
      print "Config load failed"
      print ("Server can't load '" ++ label ++ "'. Maybe the file is missing or not well-formatted")
      print error
      exitFailure

loadConfigValue :: String -> ByteString -> IO Value
loadConfigValue label bs = loadConfigWith label bs (return . either (Left . GeneralServerError . show) Right . decodeEither')
