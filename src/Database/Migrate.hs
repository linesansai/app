module Database.Migrate where
import Database.Persist.Sql (printMigration, runMigrationUnsafe, Migration, runMigration, rawExecute)
import Database.User (migrateUser)
import Core.AppMonad (runDB, runAppMonadIO)
import Control.Monad (forM, forM_)
import Core.Types.Config 
import Database.Offer (migrateOffer)

allMigrations :: [Migration]
allMigrations = [migrateUser, migrateOffer]

printMigrations :: IO ()
printMigrations = runAppMonadIO $ runDB $ printMigration migrateUser

runMigrations :: IO ()
runMigrations = forM_ allMigrations (runAppMonadIO . runDB . runMigration)

cleanupDB :: IO ()
cleanupDB = do 
    let command = "DROP SCHEMA public CASCADE; CREATE SCHEMA public;"
    runAppMonadIO $ runDB $ rawExecute command []