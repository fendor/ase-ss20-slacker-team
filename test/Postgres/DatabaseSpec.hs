module Postgres.DatabaseSpec where

import           Control.Monad
import           Test.Hspec
import           Postgres.Polysemy
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Error
import           Polysemy.Fail
import           Utils
import           Horse
import           Jockey
import           Race
import           Database.PostgreSQL.Simple
import qualified Data.Text                     as T
import           UnliftIO.Exception

runDatabase
  :: Sem
       ( DbCrud Jockey : DbCrud Horse : Error T.Text : Fail : Reader Connection : Embed IO : '[]
       )
       a
  -> IO a
runDatabase eff =
  bracket (connect defaultConnectInfo { connectUser = "baldr" }) close
    $ \conn -> do
        res <-
          runM
          . runReader conn
          . runFail
          . runError
          . runPostgresHorse
          $ runPostgresJockey eff
        case res of
          Left  err -> error err
          Right r   -> case r of
            Left  t -> error (T.unpack t)
            Right s -> return s

jockey :: Jockey
jockey = Jockey "Jack" 1.1 21 False

horse :: Horse
horse = Horse "Kato" 12.5 "no-image" False

databaseSpec :: Spec
databaseSpec = do
  describe "horse" $ do
    it "insert" $ runDatabase $ do
      res <- insert horse
      embed $ model res `shouldBe` horse

    it "find" $ runDatabase $ do
      h@(Model hid _) <- insert horse
      Just found      <- findOne hid
      embed $ found `shouldBe` h

    it "update" $ runDatabase $ do
      dbHorse <- insert horse
      let newHorse =
            dbHorse { model = (model dbHorse) { horseName = "biggie" } }
      update newHorse
      Just found <- findOne (modelId dbHorse)
      embed $ found `shouldBe` newHorse

    it "delete" $ runDatabase $ do
      Model hid _ <- insert horse
      delete @Horse hid
      Just (Model _ Horse {..}) <- findOne hid
      embed $ horseDeleted `shouldBe` True

  describe "jockey" $ do
    it "insert" $ runDatabase $ do
      res <- insert jockey
      embed $ model res `shouldBe` jockey

    it "find" $ runDatabase $ do
      j@(Model jid _) <- insert jockey
      Just f <- findOne jid
      embed $  f `shouldBe` j

    it "update" $ runDatabase $ do
      dbJockey <- insert jockey
      let newJockey = dbJockey { model = (model dbJockey) { jockeyAge = 22 } }
      update newJockey
      Just f <- findOne @Jockey (modelId dbJockey)
      embed $ f `shouldBe` newJockey

    it "delete" $ runDatabase $ do
      Model hid        _         <- insert jockey
      delete @Jockey hid
      Just (Model _ Jockey {..}) <- findOne  hid
      embed $ jockeyDeleted `shouldBe` True

  describe "race" $ do
    it "new sequence id" $ runDatabase $ do
      conn <- ask
      one <- embed $ newRace conn
      two <- embed $ newRace conn
      embed $ one `shouldNotBe` two

    it "insert" $ runDatabase $ do
      conn       <- ask
      raceId     <- embed $ newRace conn
      raceJockey <- insert jockey
      raceHorse  <- insert horse
      race       <- embed $ insertRaceEntry conn RaceEntry { .. }
      embed $ model race `shouldBe` RaceEntry { .. }

    it "create new race" $ runDatabase $ do
      (nums, _) <- createNewRace [(jockey, horse)]
      embed $ nums `shouldBe` 1

    it "find race" $ runDatabase $ do
      conn                     <- ask
      (_, rid)                 <- createNewRace [(jockey, horse)]
      [Model _ RaceEntry {..}] <- embed $ findRace conn rid
      embed $ model raceHorse `shouldBe` horse
      embed $ model raceJockey `shouldBe` jockey
      embed $ raceId `shouldBe` rid


createNewRace
  :: Members '[Reader Connection, DbCrud Horse, DbCrud Jockey, Embed IO] r
  => [(Jockey, Horse)]
  -> Sem r (Int, Int)
createNewRace entries = do
  modelEntries <- forM entries
    $ \(j, h) -> (,) <$> (modelId `fmap` insert j) <*> (modelId `fmap` insert h)
  conn     <- ask
  rid      <- embed (newRace conn)

  modified <- embed (createRace conn rid modelEntries)
  return (modified, rid)
