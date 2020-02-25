module Postgres.DatabaseSpec where

import           Control.Monad
import           Test.Hspec
import           Postgres.Simple
import           Utils
import           Horse
import           Jockey
import           Race
import           Database.PostgreSQL.Simple

withDefConnection :: (Connection -> IO a) -> IO a
withDefConnection f = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  f conn

jockey :: Jockey
jockey = Jockey "Jack" 1.1 21 False

horse :: Horse
horse = Horse "Kato" 12.5 "no-image" False

databaseSpec :: Spec
databaseSpec = do
  describe "horse" $ do
    it "insert" $ do
      res <- withDefConnection (`insert` horse)
      model res `shouldBe` horse

    it "find" $ withDefConnection $ \conn -> do
      h@(Model hid _) <- insert conn horse
      findOne conn hid `shouldReturn` Just h

    it "update" $ withDefConnection $ \conn -> do
      dbHorse <- insert conn horse
      let newHorse =
            dbHorse { model = (model dbHorse) { horseName = "biggie" } }
      update conn newHorse
      findOne conn (modelId dbHorse) `shouldReturn` Just newHorse

    it "delete" $ withDefConnection $ \conn -> do
      Model hid       _         <- insert conn horse
      (     _ :: Horse)         <- delete conn hid
      Just (Model _ Horse {..}) <- findOne conn hid
      horseDeleted `shouldBe` True

  describe "jockey" $ do
    it "insert" $ do
      res <- withDefConnection (`insert` jockey)
      model res `shouldBe` jockey

    it "find" $ withDefConnection $ \conn -> do
      j@(Model jid _) <- insert conn jockey
      findOne conn jid `shouldReturn` Just j

    it "update" $ withDefConnection $ \conn -> do
      dbJockey <- insert conn jockey
      let newJockey = dbJockey { model = (model dbJockey) { jockeyAge = 22 } }
      update conn newJockey
      findOne conn (modelId dbJockey) `shouldReturn` Just newJockey

    it "delete" $ withDefConnection $ \conn -> do
      Model hid        _         <- insert conn jockey
      (     _ :: Jockey)         <- delete conn hid
      Just (Model _ Jockey {..}) <- findOne conn hid
      jockeyDeleted `shouldBe` True

  describe "race" $ do
    it "new sequence id" $ withDefConnection $ \conn -> do
      one <- newRace conn
      two <- newRace conn
      one `shouldNotBe` two

    it "insert" $ withDefConnection $ \conn -> do
      raceId     <- newRace conn
      raceJockey <- insert conn jockey
      raceHorse  <- insert conn horse
      race       <- insertRaceEntry conn RaceEntry { .. }
      model race `shouldBe` RaceEntry { .. }

    it "create new race" $ withDefConnection $ \conn -> do
      (nums, _) <- createNewRace conn [(jockey, horse)]
      nums `shouldBe` 1

    it "find race" $ withDefConnection $ \conn -> do
      (_, rid)                 <- createNewRace conn [(jockey, horse)]
      [Model _ RaceEntry {..}] <- findRace conn rid
      model raceHorse `shouldBe` horse
      model raceJockey `shouldBe` jockey
      raceId `shouldBe` rid


createNewRace :: Connection -> [(Jockey, Horse)] -> IO (Int, Int)
createNewRace conn entries = do
  modelEntries <- forM entries $ \(j, h) ->
    (,) <$> (modelId `fmap` insert conn j) <*> (modelId `fmap` insert conn h)

  rid      <- newRace conn

  modified <- createRace conn rid modelEntries
  return (modified, rid)
