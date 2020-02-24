module Postgres.DatabaseSpec where

import           Control.Monad
import           Test.Hspec
import           Postgres.Database
import           Types
import           Database.PostgreSQL.Simple

withDefConnection :: (Connection -> IO a) -> IO a
withDefConnection f = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  f conn

jockey :: Jockey
jockey = Jockey "Jack" 1.1 21 False

horse :: Horse
horse = Horse "Kato" 12.5 "no-image" False

main :: IO ()
main = hspec $ do
  describe "horse" $ do
    it "insert" $ do
      res <- withDefConnection (`insertHorse` horse)
      model res `shouldBe` horse

    it "find" $ withDefConnection $ \conn -> do
      h@(Model hid _) <- insertHorse conn horse
      findHorse conn hid `shouldReturn` Just h

    it "update" $ withDefConnection $ \conn -> do
      dbHorse <- insertHorse conn horse
      let newHorse =
            dbHorse { model = (model dbHorse) { horseName = "biggie" } }
      updateHorse conn newHorse
      findHorse conn (modelId dbHorse) `shouldReturn` Just newHorse

  describe "jockey" $ do
    it "insert" $ do
      res <- withDefConnection (`insertJockey` jockey)
      model res `shouldBe` jockey

    it "find" $ withDefConnection $ \conn -> do
      j@(Model jid _) <- insertJockey conn jockey
      findJockey conn jid `shouldReturn` Just j

    it "update" $ withDefConnection $ \conn -> do
      dbJockey <- insertJockey conn jockey
      let newJockey = dbJockey { model = (model dbJockey) { jockeyAge = 22 } }
      updateJockey conn newJockey
      findJockey conn (modelId dbJockey) `shouldReturn` Just newJockey

  describe "race" $ do
    it "new sequence id" $ withDefConnection $ \conn -> do
      one <- newRace conn
      two <- newRace conn
      one `shouldNotBe` two

    it "insert" $ withDefConnection $ \conn -> do
      raceId     <- newRace conn
      raceJockey <- insertJockey conn jockey
      raceHorse  <- insertHorse conn horse
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
    (,)
      <$> (modelId `fmap` insertJockey conn j)
      <*> (modelId `fmap` insertHorse conn h)

  rid      <- newRace conn

  modified <- createRace conn rid modelEntries
  return (modified, rid)
