{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Show
BlogPost
  title String
  authorId PersonId
  deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll

  johnId <- insert $ Person "John Doe" $ Just 34
  janeId <- insert $ Person "Jane Doe" $ Nothing

  insert $ BlogPost "My first post" johnId
  insert $ BlogPost "Second for the road" johnId

  oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
  liftIO $ print (oneJohnPost :: [Entity BlogPost])

  john <- get johnId
  liftIO $ print (john :: Maybe Person)

  delete janeId
  deleteWhere [BlogPostAuthorId ==. johnId]

