module Postgres.Simple where

import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Types
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Fail
import           Postgres.Polysemy              ( DbCrud(..) )

runSchema
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Horse : DbCrud Jockey : r) a
  -> Sem r a
runSchema = runPostgresJockey . runPostgresHorse

runPostgresHorse
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Horse : r) a
  -> Sem r a
runPostgresHorse = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert Horse {..} -> do
      [Only hid] <- embed $ returning
        conn
        [sql|INSERT INTO HORSE (name, speed, image)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(horseName, horseSpeed, horseImage)]
      return (Model hid Horse { .. })
    FindOne hid -> do
      res <- embed $ query
        conn
        "SELECT id, name, speed, image, deleted FROM HORSE WHERE id = ?"
        (Only hid)
      return (listToMaybe res)
    FindAll -> embed $ query_
      conn
      "SELECT id, name, speed, image, deleted FROM HORSE WHERE deleted = FALSE"

    Update (Model hid Horse {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE Horse SET (name, speed, image) = (?, ?, ?) WHERE id = ?"
        (horseName, horseSpeed, horseImage, hid)
      return ()
    Delete hid -> do
      _ <- embed
        $ execute conn "UPDATE Horse SET deleted = True WHERE id = ?" (Only hid)
      return ()

runPostgresJockey
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Jockey : r) a
  -> Sem r a
runPostgresJockey = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert Jockey {..} -> do
      [Only jid] <- embed $ returning
        conn
        [sql|INSERT INTO JOCKEY (name, skill, age)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(jockeyName, jockeySkill, jockeyAge)]
      return (Model jid Jockey { .. })
    FindOne hid -> do
      res <- embed $ query
        conn
        [sql|SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?
        |]
        (Only hid)
      return (listToMaybe res)
    FindAll -> embed $ query_
      conn
      "SELECT id, name, skill, age, deleted FROM JOCKEY WHERE deleted = FALSE"

    Update (Model jid Jockey {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
        (jockeyName, jockeySkill, jockeyAge, jid)
      return ()
    Delete jid -> do
      _ <- embed $ execute conn
                           "UPDATE Jockey SET deleted = True WHERE id = ?"
                           (Only jid)
      return ()

class Crud a where
  type DbKey a
  type DbConn a
  insert :: DbConn a -> a -> IO (Model a)
  findOne :: DbConn a -> DbKey a -> IO (Maybe (Model a))
  findAll :: DbConn a -> IO [Model a]
  update :: DbConn a -> Model a -> IO ()
  delete :: DbConn a -> DbKey a -> IO a

instance Crud Horse where
  type DbKey Horse = Int
  type DbConn Horse = Connection
  insert conn Horse {..} = do
    [Only hid] <- returning
      conn
      [sql|INSERT INTO HORSE (name, speed, image)
            VALUES (?, ?, ?)
            RETURNING id
      |]
      [(horseName, horseSpeed, horseImage)]
    return (Model hid Horse { .. })

  findOne conn hid = do
    res <- query
      conn
      "SELECT id, name, speed, image, deleted FROM HORSE WHERE id = ?"
      (Only hid)
    return (listToMaybe res)

  findAll conn = query_
    conn
    "SELECT id, name, speed, image, deleted FROM HORSE WHERE deleted = FALSE"

  update conn (Model hid Horse {..}) = do
    _ <- execute
      conn
      "UPDATE Horse SET (name, speed, image) = (?, ?, ?) WHERE id = ?"
      (horseName, horseSpeed, horseImage, hid)
    return ()

  delete conn hid = do
    _ <- execute conn "UPDATE Horse SET deleted = True WHERE id = ?" (Only hid)
    return undefined

instance Crud Jockey where
  type DbKey Jockey = Int
  type DbConn Jockey = Connection
  insert conn Jockey {..} = do
    [Only jid] <- returning
      conn
      [sql|INSERT INTO JOCKEY (name, skill, age)
            VALUES (?, ?, ?)
            RETURNING id
      |]
      [(jockeyName, jockeySkill, jockeyAge)]
    return (Model jid Jockey { .. })

  findOne conn hid = do
    res <- query
      conn
      [sql|SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?
      |]
      (Only hid)
    return (listToMaybe res)

  findAll conn = query_
    conn
    "SELECT id, name, skill, age, deleted FROM JOCKEY WHERE deleted = FALSE"


  update conn (Model jid Jockey {..}) = do
    _ <- execute
      conn
      "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
      (jockeyName, jockeySkill, jockeyAge, jid)
    return ()

  delete conn jid = do
    _ <- execute conn "UPDATE Jockey SET deleted = True WHERE id = ?" (Only jid)
    return undefined


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

newRace :: Connection -> IO Int
newRace conn = do
  [Only rid] <- query_ conn "SELECT nextval('race_entry_seq')"
  return rid


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
