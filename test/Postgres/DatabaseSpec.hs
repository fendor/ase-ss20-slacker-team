module Postgres.DatabaseSpec where


import           Test.Hspec
import           Postgres.Database
import           Database.PostgreSQL.Simple

withDefConnection :: (Connection -> IO a) -> IO a
withDefConnection f = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  f conn

jockey :: Jockey
jockey = Jockey "Jack" 1.0 21 False

horse :: Horse
horse = Horse "Kato" 12.0 "no-image" False

main :: IO ()
main = hspec $ do
  describe "horse" $ do
    it "insert" $ do
      res <- withDefConnection (`insertHorse` horse)
      model res `shouldBe` horse

    it "find" $
      withDefConnection $ \conn -> do
        h@(Model hid _) <- insertHorse conn horse
        findHorse conn hid `shouldReturn` Just h

    it "update" $
      withDefConnection $ \conn -> do
        dbHorse <- insertHorse conn horse
        let newHorse = dbHorse { model = (model dbHorse) { horseName = "biggie" } }
        updateHorse conn newHorse
        findHorse conn (modelId dbHorse) `shouldReturn` Just newHorse

  describe "jockey" $ do
    it "insert" $ do
      res <- withDefConnection (`insertJockey` jockey)
      model res `shouldBe` jockey

    it "find" $
      withDefConnection $ \conn -> do
        j@(Model jid _) <- insertJockey conn jockey
        findJockey conn jid `shouldReturn` Just j

    it "update" $
      withDefConnection $ \conn -> do
        dbJockey <- insertJockey conn jockey
        let newJockey = dbJockey { model = (model dbJockey) { jockeyAge = 22 } }
        updateJockey conn newJockey
        findJockey conn (modelId dbJockey) `shouldReturn` Just newJockey

  describe "race" $ do
    it "new sequence id" $
      withConnection $ \conn -> do
        one <- newRace conn
        two <- newRace conn
        one `shouldNotBe` two

    it "insert" $
      withDefConnection
        (\conn -> do
          raceId     <- newRace conn
          raceJockey <- insertJockey conn jockey
          raceHorse  <- insertHorse conn horse

          race       <- insertRaceEntry conn RaceEntry { .. }

          model race `shouldBe` RaceEntry { .. }
        )




