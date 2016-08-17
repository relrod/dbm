{-# LANGUAGE OverloadedStrings #-}
module SQLite where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple

import Utility

instance FromRow Migration where
  fromRow = Migration <$> field <*> field <*> field

instance ToRow Migration where
  toRow (Migration id_ timestamp mid) = toRow (id_, timestamp, mid)

-- These are slightly inefficient right now in that they open and close a
-- connection each time. This is probably worth optimizing later, but especially
-- moreso for the other backends where network overhead is a legitimate concern.

initialize :: FilePath -> IO ()
initialize path = do
  conn <- open path
  execute_ conn $
    "create table if not exists dbm_migrations (\
    \  id integer primary key autoincrement\
    \, applied_at datetime not null default current_timestamp\
    \, migration_id integer not null\
    \);"
  close conn

logMigration :: Connection -> Integer -> IO ()
logMigration conn mid =
  execute conn "insert into dbm_migrations (migration_id) values (?);" (Only mid)

performMigration :: FilePath -> FilePath -> Integer -> IO ()
performMigration dbPath sql mid = do
  conn <- open dbPath
  withTransaction conn $ do
    migrationQuery <- T.readFile sql
    execute_ conn (Query migrationQuery)
    logMigration conn mid

getLastMigration :: FilePath -> IO (Maybe Integer)
getLastMigration dbPath = do
  conn <- open dbPath
  lastMigration <- query_ conn "select * from dbm_migrations order by id desc limit 1;" :: IO [Migration]
  return (listToMaybe (mid <$> lastMigration))
