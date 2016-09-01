-- | Global types and functions.
module Utility where

import Data.Time (UTCTime)
import Text.Read

data Migration =
  Migration { id_ :: Integer
            , timestamp :: UTCTime
            , mid :: Integer
            } deriving (Show)

parseMigrationId :: String -> Maybe Integer
parseMigrationId = readMaybe . takeWhile (/= '-')
