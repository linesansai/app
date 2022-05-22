module Main where

import Control.Monad
import Data.List (intercalate)
import Database.Migrate (cleanupDB, runMigrations)
import Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  maybe printCommands runCommand (lookupCommand args)
  where
    lookupCommand args = lookup args commandsByArgs
    commandsByArgs = map (\a -> (commandArgs a, a)) allCommands
    printCommands = do
      putStrLn "Available commands: "
      forM_ allCommands \cmd -> do
        let argsStr = unwords (commandArgs cmd)
        let descriptionStr = commandDescription cmd
        putStrLn ("\t\ESC[32m" <> argsStr <> "\ESC[0m " <> descriptionStr)

data Command = RunServer | RunMigrations | CleanupDB
  deriving (Bounded, Enum)

allCommands :: [Command]
allCommands = [minBound .. maxBound]

runCommand :: Command -> IO ()
runCommand RunServer = runServer
runCommand RunMigrations = runMigrations
runCommand CleanupDB = cleanupDB

commandArgs :: Command -> [String]
commandArgs RunServer = ["runServer"]
commandArgs RunMigrations = ["runMigrations"]
commandArgs CleanupDB = ["cleanup"]

commandDescription :: Command -> String
commandDescription RunServer = "To run server"
commandDescription RunMigrations = "To run DB migrations"
commandDescription CleanupDB = "To cleanup DB (remove tables)"

pattern RunServerCmd :: [String]
pattern RunServerCmd = ["run", "server"]

pattern RunMigrationsCmd :: [String]
pattern RunMigrationsCmd = ["run", "migrations"]

pattern CleanupDBCmd :: [String]
pattern CleanupDBCmd = ["cleanup"]