module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
-- import Lib

data Tool = Tool 
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
}

instance Show Tool where
  show tool =
    mconcat [
        show $ toolId tool
      , ") "
      , name tool
      , "\n description: "
      , description tool
      , "\n last returned: "
      , show $ lastReturned tool
      , "\n times borrowed: "
      , show $ timesBorrowed tool
      , "\n"
    ]

----------
data User = User
  { userId    :: Int
  , userName  :: String
  }

instance Show User where
  show user =
    mconcat [
        show $ userId user
      , ") "
      , userName user 
    ]

----------
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

----------
addUser :: String -> IO ()
addUser userName = 
  withConn "tools.db" $
  \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
    print "user added"
  
----------
checkout :: Int -> Int -> IO ()
checkout userId toolId = 
  withConn "tools.db" $
  \conn -> do
    execute conn 
      "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)" (userId, toolId)

----------
-- Make User and Tool instances of FromRow, so that raw row data from the
-- database can be converted to Haskell data types.

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

----------
printUsers :: IO ()
printUsers = 
  withConn "tools.db" $
  \conn -> do
    resp <- query_ conn "SELECT * FROM USERS;" :: IO [User]
    mapM_ print resp

----------
printToolQuery :: Query -> IO ()
printToolQuery q = 
  withConn "tools.db" $
  \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

----------
printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

----------
printAvailable :: IO ()
printAvailable = 
  printToolQuery $ mconcat [
    "select * from tools "
  , "where id not in "
  , "(select tool_id from checkedout);"]

----------
printCheckedout :: IO ()
printCheckedout = 
  printToolQuery $ mconcat [
    "select * from tools "
  , "where id in "
  , "(select tool_id from checkedout);"]

----------
-- Update existing data
-- Select a tool from the database by its id
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

----------
firstOrNothing :: [a] -> Maybe a
firstOrNothing []     = Nothing
firstOrNothing (x:_)  = Just x

----------
-- Update the given tool's date and times borrowed
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = 1 + timesBorrowed tool
  }

----------
-- Updates the database if the tool is present, otherwise prints a warning
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "Tool id not found"
updateOrWarn (Just tool) =
  withConn "tools.db" $
  \conn -> do
    let q = "UPDATE tools SET lastReturned = ?, timesBorrowed = ? WHERE id = ?;"
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    print "Tool table updated"

----------
-- Given a tool id, safely update the tool table
updateToolTable :: Int -> IO ()
updateToolTable toolId = 
  withConn "tools.db" $
  \conn -> do
    tool        <- selectTool conn toolId
    currentDay  <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool
                                 <*> pure currentDay
    updateOrWarn updatedTool

----------
-- Check in a tool, which means delete it from the checkedout table
checkin :: Int -> IO ()
checkin toolId =
  withConn "tools.db" $
  \conn -> do
    execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

----------
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

----------
-- Wrap all the database actions in a usable interface
promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName


----------
promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- pure read <*> getLine
  print "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

----------
promptAndCheckin :: IO ()
promptAndCheckin = do
  print "Enter the id of the tool to check in"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

----------
performCommand :: String -> IO ()
performCommand command = 
  case command of
    "users"     -> printUsers         >> main
    "tools"     -> printTools         >> main
    "addUser"   -> promptAndAddUser   >> main
    "checkout"  -> promptAndCheckout  >> main
    "checkin"   -> promptAndCheckin   >> main
    "in"        -> printAvailable     >> main
    "out"       -> printCheckedout    >> main
    "quit"      -> print "Bye!"
    _           -> print "Sorry, command not found" >> main

----------
main :: IO ()
main = do
  print "Enter a command (quit/users/tools/addUser/checkout/checkin/in/out)"
  command <- getLine
  performCommand command
