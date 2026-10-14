module Specs.Database.DAO.CommonSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.DAO.Common
import Shared.Model.Common.Sort

daoCommonSpec =
  describe "DAO Common" $ do
    describe "mapSort" $
      it "renders every sort in one ORDER BY" $
        mapSort [Sort "name" Ascending, Sort "createdAt" Descending]
          `shouldBe` "ORDER BY name asc , created_at desc "
    describe "mapSortWithCustomMapping" $ do
      it "renders every sort in one ORDER BY" $
        mapSortWithCustomMapping [Sort "name" Ascending, Sort "createdAt" Descending] []
          `shouldBe` "ORDER BY name asc , created_at desc "
      it "applies the custom mapping to every sort" $
        mapSortWithCustomMapping [Sort "name" Ascending, Sort "owner" Descending] [("owner", "owner_name")]
          `shouldBe` "ORDER BY name asc , owner_name desc "
