module Krinj.UserService.Command (
    createUser
) where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Krinj.UserService.Types
import Krinj.UserService.Model (CredentialsRow(..), credentialsTable, UsersRow(..), usersTable)
import Krinj.UserService.Hash (hashPassword)
import Opaleye ((.&&), (.==), Insert(..), rCount, rReturning, runInsert, runSelect, selectTable, toFields, where_)
import Opaleye.Exists (exists)

toUser :: [(UserId,EmailAddress)] -> Maybe User
toUser = fmap (uncurry User) . listToMaybe

createUser :: Connection -> EmailAddress -> Password -> IO (Maybe User)
createUser conn emailAddress password = withTransaction conn $ do
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
        p <- hashPassword password
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

