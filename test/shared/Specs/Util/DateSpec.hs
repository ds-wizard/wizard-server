module Specs.Util.DateSpec where

import Data.Time
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Date

dateSpec =
  describe "Date" $
    describe "truncateToMicroseconds" $ do
      it "drops the nanoseconds the database does not store" $
        truncateToMicroseconds (UTCTime (fromGregorian 2026 10 19) 76751.631632722) `shouldBe` UTCTime (fromGregorian 2026 10 19) 76751.631632
      it "keeps a time in microseconds" $
        truncateToMicroseconds (UTCTime (fromGregorian 2026 10 19) 76751.631632) `shouldBe` UTCTime (fromGregorian 2026 10 19) 76751.631632
