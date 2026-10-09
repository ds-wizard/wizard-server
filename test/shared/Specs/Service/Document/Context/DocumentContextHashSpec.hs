module Specs.Service.Document.Context.DocumentContextHashSpec where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.String (fromString)
import qualified Data.Text as T
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Service.Document.Context.DocumentContextHash

documentContextHashSpec =
  describe "computeContextValueHash" $ do
    it "matches the pinned hash of the project fixture" $ do
      ctx <- loadFixture projectFixture
      computeContextValueHash ctx `shouldBe` projectHash
    it "matches the pinned hash of the KM editor fixture" $ do
      ctx <- loadFixture kmEditorFixture
      computeContextValueHash ctx `shouldBe` kmEditorHash
    it "matches the pinned hash of the worker fixture" $ do
      ctx <- loadFixture workerFixture
      computeContextValueHash ctx `shouldBe` projectHash
    it "matches the pinned hash of the edge fixture" $ do
      ctx <- loadFixture edgeFixture
      computeContextValueHash ctx `shouldBe` edgeHash
    it "equals the fixture's own hash field" $
      mapM_
        ( \fixture -> do
            ctx <- loadFixture fixture
            Just (String (T.pack (computeContextValueHash ctx))) `shouldBe` lookupKey "hash" ctx
        )
        [projectFixture, kmEditorFixture, workerFixture, edgeFixture]
    it "ignores the document" $ do
      ctx <- loadFixture projectFixture
      computeContextValueHash (setKey "document" (String "anything") ctx) `shouldBe` projectHash
      computeContextValueHash (deleteKey "document" ctx) `shouldBe` projectHash
    it "ignores the hash field itself" $ do
      ctx <- loadFixture projectFixture
      computeContextValueHash (setKey "hash" (String "sha256:0") ctx) `shouldBe` projectHash
      computeContextValueHash (deleteKey "hash" ctx) `shouldBe` projectHash
    it "ignores the knowledge model when a project is present" $ do
      ctx <- loadFixture projectFixture
      computeContextValueHash (setKey "knowledgeModel" Null ctx) `shouldBe` projectHash
    it "hashes the knowledge model when the project is null" $ do
      ctx <- loadFixture kmEditorFixture
      computeContextValueHash (setKey "knowledgeModel" Null ctx) `shouldNotBe` kmEditorHash
    it "changes when a reply changes" $ do
      ctx <- loadFixture projectFixture
      let changed = updateKey "questionnaire" (updateKey "replies" (mapObject (fmap (const Null)))) ctx
      computeContextValueHash changed `shouldNotBe` projectHash

projectFixture, kmEditorFixture, workerFixture, edgeFixture :: FilePath
projectFixture = "test/shared/Fixtures/document_context_hash_fixture.json"
kmEditorFixture = "test/shared/Fixtures/document_context_km_editor_fixture.json"
workerFixture = "test/shared/Fixtures/document_context_worker_fixture.json"
edgeFixture = "test/shared/Fixtures/document_context_hash_edge_fixture.json"

projectHash, kmEditorHash, edgeHash :: String
projectHash = "sha256:138582554e1abfd6ab46579d563f9ee19b7935de6908f0314556c577ffb15e65"
kmEditorHash = "sha256:08c62b1530cb7a6f526e5bd1e941d72a921665285df1c4d5454997b1ad9806b7"
edgeHash = "sha256:7357368c501cc707de06a6b915939c073e77438b053e46ebc03a82a595e1190c"

loadFixture :: FilePath -> IO Value
loadFixture path = do
  bytes <- BS.readFile path
  maybe (fail ("not valid JSON: " ++ path)) return (decodeStrict bytes)

lookupKey :: String -> Value -> Maybe Value
lookupKey key (Object o) = KM.lookup (fromString key) o
lookupKey _ _ = Nothing

setKey :: String -> Value -> Value -> Value
setKey key value (Object o) = Object (KM.insert (fromString key) value o)
setKey _ _ value = value

deleteKey :: String -> Value -> Value
deleteKey key (Object o) = Object (KM.delete (fromString key) o)
deleteKey _ value = value

updateKey :: String -> (Value -> Value) -> Value -> Value
updateKey key f = mapObject (KM.mapWithKey (\k v -> if k == fromString key then f v else v))

mapObject :: (KM.KeyMap Value -> KM.KeyMap Value) -> Value -> Value
mapObject f (Object o) = Object (f o)
mapObject _ value = value
