module Krinj.UserService.Command (
    createUser
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Krinj.UserService.Model (CredentialsRow(..), credentialsTable, UsersRow(..), usersTable)
import Krinj.UserService.Hash (hashPassword)
import Krinj.UserService.Types (EmailAddress(..), Password, Salt, User(..), UserId)
import Opaleye ((.&&), (.==), Insert(..), rCount, rReturning, runInsert, runSelect, selectTable, toFields, where_)
import Opaleye.Exists (exists)

toUser :: [(UserId,EmailAddress)] -> Maybe User
toUser = fmap (uncurry User) . listToMaybe

createUser :: Connection -> EmailAddress -> Password -> Salt -> IO (Maybe User)
createUser conn (EmailAddress emailAddress) password salt = withTransaction conn $ do
  [emailAddressIsUsed] :: [Bool] <- runSelect conn $ exists $ do
    row <- selectTable usersTable
    where_ $ row.emailAddress .== toFields emailAddress
         .&& row.isDeleted .== toFields False
    pure ()
  if emailAddressIsUsed
  then pure Nothing
  else do
    result <- fmap toUser $ runInsert conn $
      Insert
      { iTable = usersTable
      , iRows = [UsersRow
                 { userId = Nothing
                 , emailAddress = toFields emailAddress
                 , isVerified = Nothing
                 , isDeleted = Nothing }]
      , iReturning = rReturning $ \row -> (row.userId, row.emailAddress)
      , iOnConflict = Nothing
      }
    case result of
      Nothing -> pure Nothing
      Just user -> do
        let p = hashPassword password salt
        _ <- runInsert conn $
          Insert
          { iTable = credentialsTable
          , iRows = [CredentialsRow { userId = toFields (Just user.userId)
                                    , passwordHash = toFields (Just p)
                                    , isDeleted = Nothing }]
          , iReturning = rCount
          , iOnConflict = Nothing
          }
        pure result

