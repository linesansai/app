{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.User where
import Database.Persist.TH
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto.Legacy
import Data.Hashable (hash)
import Data.Time (getCurrentTime)
import UnliftIO (liftIO)
import Database.Esqueleto.Internal.Internal ( Update )
import Data.OpenApi (ToSchema, ToParamSchema)

share
  [mkPersist sqlSettings, mkMigrate "migrateUser"]
  [persistLowerCase|
User 
    createdAt Time.UTCTime
    active Bool
    name T.Text
    login T.Text
    email T.Text
    balance Int
    phoneNumber T.Text
    passHash Int
    isEmailConfirmed Bool
    deriving Show Eq
    UniqueLogin login
    UniqueEmail email
  |]
deriving via Int instance ToSchema UserId 
deriving via Int instance ToParamSchema UserId 

data CreateUser = CreateUser
  {
    name :: T.Text,
    login :: T.Text,
    email :: T.Text,
    password :: T.Text,
    phoneNumber :: T.Text
  }

createUser ::  CreateUser -> SqlPersistM (Either (Entity User) (Entity User))
createUser CreateUser {..} = do 
  now <- liftIO getCurrentTime
  let record = User 
        {
          userCreatedAt = now,
          userActive = True,
          userName = name,
          userLogin = login,
          userEmail = email,
          userBalance = 500,
          userPhoneNumber = phoneNumber,
          userPassHash = hash password,
          userIsEmailConfirmed = False
        }
  fmap (`Entity` record) <$> insertBy record 

updateUser :: 
  UserId
  -> [SqlExpr (Entity User) -> SqlExpr Update]
  ->  SqlPersistM ()
updateUser userId updates = update $ \ca -> do
    set ca updates
    where_ $ ca ^. UserId ==. val userId

loadUserByLoginAndPassword :: 
  T.Text
  -> T.Text
  -> SqlPersistM (Maybe (Entity User))
loadUserByLoginAndPassword login password = 
  selectOne $ from \user -> do
    where_ $ user ^. #login ==. val login
            &&. user ^. #passHash ==. val passHash
    pure user
  where 
    passHash = hash password

loadUserById :: UserId -> SqlPersistM (Maybe (Entity User))
loadUserById userId = fmap (Entity userId) <$> get userId

loadUserByLogin :: T.Text -> SqlPersistM (Maybe (Entity User))
loadUserByLogin login = getBy (UniqueLogin login)

loadUserByEmail :: T.Text -> SqlPersistM (Maybe (Entity User))
loadUserByEmail email = getBy (UniqueEmail email)