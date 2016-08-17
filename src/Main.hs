{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Ini
import Data.List (elem, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Exit
import Options.Applicative

import Utility

--------------------------------------------------------------------------------
-- These are the things you need to extend if you wish to add another backend. -
--------------------------------------------------------------------------------
import qualified SQLite

data Config =
    SQLiteConfig { sqliteConfigPath :: T.Text }

getConfigForEnv :: String -> Ini -> IO Config
getConfigForEnv env ini = do
  backend <- getConfigValue ini (T.pack env) "backend"
  case backend of
    "sqlite" -> SQLiteConfig <$> getConfigValue ini (T.pack env) "path"
    _ -> error "No valid backend configured."

initializeEnv :: Config -> IO ()
initializeEnv (SQLiteConfig path) = SQLite.initialize (T.unpack path)

getLastMigration :: Config -> IO (Maybe Integer)
getLastMigration (SQLiteConfig path) = SQLite.getLastMigration (T.unpack path)

doMigration :: Config -> FilePath -> Integer -> IO ()
doMigration (SQLiteConfig path) sql mid =
  SQLite.performMigration (T.unpack path) sql mid
--------------------------------------------------------------------------------

data Command =
    Configs
  | Initialize String
  | Last String
  | Status String
  | Migrate String

envParser :: Parser String
envParser = argument str (metavar "ENVIRONMENT"
                          <> help "The configuration environment to initialize")

initializeParser, lastParser, statusParser, migrateParser :: Parser Command
initializeParser = Initialize <$> envParser
lastParser = Last <$> envParser
statusParser = Status <$> envParser
migrateParser = Migrate <$> envParser

parser :: Parser Command
parser = subparser $
         (command "configs"
          (info (pure Configs)
           (progDesc "Print available configurations.")))
         <> (command "initialize"
             (info initializeParser
              (progDesc "Set up a database for use with dbm and apply all migrations.")))
         <> (command "last"
             (info lastParser
              (progDesc "Get the id of the last migration performed.")))
         <> (command "status"
             (info statusParser
              (progDesc "Get the current migration status.")))
         <> (command "migrate"
             (info migrateParser
              (progDesc "Migrate the database completely.")))

run :: Command -> Ini -> IO ()
run Configs ini = showConfigs ini
run (Initialize env) ini = getConfigForEnv env ini >>= initializeEnv
run (Last env) ini = getConfigForEnv env ini >>= showLastMigration
run (Status env) ini = getConfigForEnv env ini >>= showMigrationStatus
run (Migrate env) ini = getConfigForEnv env ini >>= performMigrations

opts :: ParserInfo Command
opts = info (parser <**> helper) idm

main :: IO ()
main = do
  current <- listDirectory "."
  if not ("sql" `elem` current)
    then do
      putStrLn "`dbm` must be run in a directory with an 'sql' subdirectory."
      exitFailure
    else do
      ini <- getConfigFile
      opts <- execParser opts
      run opts ini

getConfigFile :: IO Ini
getConfigFile = do
  config <- readIniFile "sql/.dbm"
  case config of
    Right x -> return x
    Left err -> putStrLn err >> exitFailure

getConfigValue :: Ini -> T.Text -> T.Text -> IO T.Text
getConfigValue c s l = do
  case lookupValue s l c of
    Right x -> return x
    Left err -> putStrLn err >> exitFailure

showConfigs :: Ini -> IO ()
showConfigs config = do
  mapM_ T.putStrLn (sections config)

showLastMigration :: Config -> IO ()
showLastMigration cfg = do
  mid <- getLastMigration cfg
  case mid of
    Just mid -> putStrLn $ "Last migration id: " ++ show mid
    Nothing -> putStrLn "No migrations have been performed. :-("

getNecessaryMigrations :: Config -> IO [(Integer, FilePath)]
getNecessaryMigrations cfg = do
  mid <- fromMaybe 0 <$> getLastMigration cfg
  migrations <- filter (\x -> ".sql" `isSuffixOf` x) <$> listDirectory "sql"
  let migrationList = fmap (\x -> (parseMigrationId x, "sql/" ++ x)) migrations
  migrationFiltered <- filterM (getNecessaryMigrations' mid) migrationList
  return [(x, y) | (Just x, y) <- migrationFiltered]
  where
    getNecessaryMigrations' _ (Nothing, fn) = do
      putStrLn $ "Invalid migration filename: " ++ fn
      exitFailure >> return False
    getNecessaryMigrations' current (Just mid, fn) = return (mid > current)

showMigrationStatus :: Config -> IO ()
showMigrationStatus cfg = do
  migrations <- getNecessaryMigrations cfg
  if null migrations
    then putStrLn "Database schema is up to date."
    else do
      putStrLn "Need to migrate:"
      mapM_ (\(_, x) -> putStrLn ("* " ++ x)) migrations

performMigrations :: Config -> IO ()
performMigrations cfg = do
  showMigrationStatus cfg
  migrations <- getNecessaryMigrations cfg
  -- This pattern match is "safe" because we `error` in getNecessaryMigrations
  -- if it were never a `Nothing`.
  mapM_ (\(mid, x) -> doMigration' x mid) migrations
  where
    doMigration' sql mid = do
      putStr $ "Migrating " ++ sql ++ "... "
      doMigration cfg sql mid
      putStrLn "Done."
