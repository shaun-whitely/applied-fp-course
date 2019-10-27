{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad                      ((>=>))
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = asks (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB aeb cioa = do
  conn <- getDBConn
  let ioea = first DBError <$> Sql.runDBAction (cioa conn)
  a <- (liftIO >=> liftEither) ioea
  liftEither $ aeb a

getComments
  :: Topic
  -> App [Comment]
getComments topic =
  let
    q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    params = Sql.Only (getTopic topic)
  in
    runDB (traverse fromDBComment) (\conn -> Sql.query conn q params)

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic topic comment = do
  nowish <- liftIO getCurrentTime
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  let params = (getTopic topic, getCommentText comment, nowish) 
  runDB Right (\conn -> Sql.execute conn q params)

getTopics
  :: App [Topic]
getTopics =
  let
    q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse (mkTopic . Sql.fromOnly)) (\conn -> Sql.query_ conn q)

deleteTopic
  :: Topic
  -> App ()
deleteTopic topic =
  let
    q = "DELETE FROM comments WHERE topic = ?"
    params = Sql.Only (getTopic topic)
  in
    runDB Right (\conn -> Sql.execute conn q params)

-- Go on to 'src/Level07/Core.hs' next.
