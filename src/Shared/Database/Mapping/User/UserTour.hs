module Shared.Database.Mapping.User.UserTour where

import Database.PostgreSQL.Simple

import Shared.Model.User.UserTour

instance ToRow UserTour

instance FromRow UserTour
