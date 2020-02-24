module Postgres.Database where

import           GHC.Generics
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Maybe
import           Data.Int                       ( Int64 )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import Types

instance FromRow Jockey
instance FromRow Horse
instance ToRow Jockey
instance ToRow Horse

instance FromRow a => FromRow (Model a) where
    fromRow = Model <$> field <*> fromRow

instance FromRow RaceEntry where
    fromRow = RaceEntry <$> fromRow <*> fromRow <*> field

insertHorse :: Connection -> Horse -> IO (Model Horse)
insertHorse conn Horse {..} = do
    [Only hid] <- returning
        conn
        [sql|INSERT INTO HORSE (name, speed, image)
             VALUES (?, ?, ?)
             RETURNING id
        |]
        [(horseName, horseSpeed, horseImage)]
    return (Model hid Horse { .. })

insertJockey :: Connection -> Jockey -> IO (Model Jockey)
insertJockey conn Jockey {..} = do
    [Only jid] <- returning
        conn
        [sql|INSERT INTO JOCKEY (name, skill, age)
             VALUES (?, ?, ?)
             RETURNING id
        |]
        [(jockeyName, jockeySkill, jockeyAge)]
    return (Model jid Jockey { .. })

insertRaceEntry :: Connection -> RaceEntry -> IO (Model RaceEntry)
insertRaceEntry conn RaceEntry {..} = do
    [Only rid] <- returning
        conn
        [sql|INSERT INTO RACEENTRY (jockey, horse, race)
             VALUES (?, ?, ?)
             RETURNING id
        |]
        [(modelId raceJockey, modelId raceHorse, raceId)]
    return (Model rid RaceEntry { .. })

createRace :: Connection -> Int -> [(Int, Int)] -> IO Int
createRace conn rid entries = do
    let packTuple :: (Int, Int) -> (Int, Int, Int)
        packTuple (jid, hid) = (jid, hid, rid)
    nums <- executeMany
        conn
        [sql|INSERT INTO RACEENTRY (jockey, horse, race)
             VALUES (?, ?, ?)
        |]
        (map packTuple entries)
    return $ fromIntegral nums

findHorse :: Connection -> Int -> IO (Maybe (Model Horse))
findHorse conn hid = do
    res <- query
        conn
        "SELECT id, name, speed, image, deleted FROM HORSE WHERE id = ?"
        (Only hid)
    return (listToMaybe res)

findJockey :: Connection -> Int -> IO (Maybe (Model Jockey))
findJockey conn hid = do
    res <- query
        conn
        [sql| SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?
        |]
        (Only hid)
    return (listToMaybe res)

findRace :: Connection -> Int -> IO [Model RaceEntry]
findRace conn rid = query
    conn
    [sql|
        SELECT r.id, j.id, j.name, j.skill, j.age, j.deleted, h.id, h.name, h.speed, h.image, h.deleted, r.race
        FROM RACEENTRY r
        JOIN JOCKEY j ON r.jockey = j.id JOIN HORSE h on h.id = r.horse
        WHERE r.race = ?
    |]
    (Only rid)

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
