module Krinj.UserService.Query (
    findUserById, findUserByCredentials
) where

import Data.Maybe (listToMaybe)
import Krinj.UserService.Hash
import Krinj.UserService.Model
import Krinj.UserService.Types
import Database.PostgreSQL.Simple (Connection)
import Opaleye ((.==), (.===), (.&&), nullableToMaybeFields, runSelect, selectTable, toFields, where_)

toUser :: [(UserId,EmailAddress)] -> Maybe User
toUser = fmap (uncurry User) . listToMaybe

findUserById :: Connection -> UserId -> IO (Maybe User)
findUserById conn userId = fmap toUser $ runSelect conn $ do
  row <- selectTable usersTable
  where_ $ row.userId .== toFields userId
       .&& row.isDeleted .== toFields False
  pure (row.userId, row.emailAddress)

findUserByCredentials :: Connection -> EmailAddress -> Password -> IO (Maybe User)
findUserByCredentials conn emailAddress password = do
  p <- hashPassword password
  fmap toUser $ runSelect conn $ do
    usersRow <- selectTable usersTable
    credentialsRow <- selectTable credentialsTable
    where_ $ usersRow.emailAddress .== toFields emailAddress
         .&& usersRow.isDeleted .== toFields False
         .&& credentialsRow.userId .== usersRow.userId
         .&& credentialsRow.isDeleted .== toFields False
         .&& nullableToMaybeFields credentialsRow.passwordHash .=== toFields (Just p)
    pure (usersRow.userId, usersRow.emailAddress)

