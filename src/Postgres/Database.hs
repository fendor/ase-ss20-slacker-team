module Postgres.Database where

import           GHC.Generics
import           Data.Text
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

data Horse = Horse
    { horseName :: Text
    , horseSpeed :: Double
    , horseImage :: Text
    , horseDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, FromRow, ToRow)


data Jockey = Jockey
    { jockeyName :: Text
    , jockeySkill :: Double
    , jockeyAge :: Int
    , jockeyDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic,  FromRow, ToRow)

data RaceEntry = RaceEntry
    { raceJockey :: Model Jockey
    , raceHorse :: Model Horse
    , raceId :: Int
    }
    deriving (Show, Eq, Read, Generic)

data Model a = Model
    { modelId :: Int
    , model :: a
    }
    deriving (Show, Eq, Read, Generic)

instance FromRow a => FromRow (Model a) where
    fromRow = Model <$> field <*> fromRow

insertHorse :: Connection -> Horse -> IO (Model Horse)
insertHorse conn Horse {..} = do
    [Only hid] <- returning
        conn
        "INSERT INTO HORSE (name, speed, image) VALUES (?, ?, ?) RETURNING id"
        [(horseName, horseSpeed, horseImage)]
    return (Model hid Horse { .. })

insertJockey :: Connection -> Jockey -> IO (Model Jockey)
insertJockey conn Jockey {..} = do
    [Only jid] <- returning
        conn
        "INSERT INTO JOCKEY (name, skill, age) VALUES (?, ?, ?) RETURNING id"
        [(jockeyName, jockeySkill, jockeyAge)]
    return (Model jid Jockey { .. })

insertRaceEntry :: Connection -> RaceEntry -> IO (Model RaceEntry)
insertRaceEntry conn RaceEntry {..} = do
    [Only rid] <- returning
        conn
        "INSERT INTO RACEENTRY (jockey, horse, race) VALUES (?, ?, ?) RETURNING id"
        [(modelId raceJockey, modelId raceHorse, raceId)]
    return (Model rid RaceEntry { .. })

findHorse :: Connection -> Int -> IO (Maybe (Model Horse))
findHorse conn hid = do
    res <- query conn "SELECT (id, name, speed, image, deleted) FROM HORSE WHERE id = ?" (Only hid)
    return (listToMaybe res)

findJockey :: Connection -> Int -> IO (Maybe (Model Jockey))
findJockey conn hid = do
    res <- query conn "SELECT (id, name, skill, age, deleted) FROM JOCKEY WHERE id = ?" (Only hid)
    return (listToMaybe res)

updateHorse :: Connection -> Model Horse -> IO ()
updateHorse conn (Model hid Horse {..}) = do
    _ <- execute
        conn
        "UPDATE Horse SET (name, speed, image) = (?, ?, ?) WHERE id = ?"
        (horseName, horseSpeed, horseImage, hid)
    return ()

updateJockey :: Connection -> Model Jockey -> IO ()
updateJockey conn (Model jid Jockey {..}) = do
    _ <- execute
        conn
        "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
        (jockeyName, jockeySkill, jockeyAge, jid)
    return ()

newRace :: Connection -> IO Int
newRace conn = do
    [Only rid] <- query_ conn "SELECT nextval('race_entry_seq')"
    return rid
