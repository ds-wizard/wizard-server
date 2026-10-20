module Shared.Model.Project.ProjectUtil where

import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Util.String (splitOn)

createReplyKey :: [U.UUID] -> String
createReplyKey = L.intercalate "." . fmap U.toString

readReplyKey :: String -> Maybe [U.UUID]
readReplyKey = traverse U.fromString . splitOn "."

replyKeyContains :: String -> U.UUID -> Bool
replyKeyContains replyKey uuid = maybe False (elem uuid) (readReplyKey replyKey)
