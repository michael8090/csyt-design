{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist as P -- We'll be using P.get later for GET /artwork/<id>.
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

-- todo: creatTime and editTime
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Artwork json
    title Text
    content Text
    deriving Show
|]

main :: IO ()
main = do 
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)

-- helpers
runSql
    :: (HasSpock m, SpockConn m ~ SqlBackend)
    => SqlPersistT (LoggingT IO) a -> m a
runSql action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
    json $
        object [
            "result" .= String "failure",
            "error" .= object [
                "code" .= code,
                "message" .= message
            ]
        ]

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a
app :: Api
app = do
    get "artworks" $ do
        allArtworks <- runSql $ selectList [] [Asc ArtworkId]
        json allArtworks
    -- read
    get ("artwork" <//> var) $ \artworkId -> do
        maybeArtwork <- runSql $ P.get artworkId :: ApiAction (Maybe Artwork)
        case maybeArtwork of
            Nothing -> errorJson 2 $ pack $ "Could not find an artwork with matching id" ++ (show artworkId)
            Just theArtwork -> json theArtwork
    -- create
    -- todo: id should be hashed, escape the input
    post "artwork" $ do
        maybeArtwork <- jsonBody :: ApiAction (Maybe Artwork)
        case maybeArtwork of
            Nothing -> errorJson 1 "Failed to parse request body as Artwork"
            Just newArtwork -> do
                newId <- runSql $ insert newArtwork
                json $ object ["result" .= String "success", "id" .= newId]
    -- update
    -- todo: escape the input
    post ("artwork" <//> var) $ \artworkId -> do
        maybeArtwork <- jsonBody :: ApiAction (Maybe Artwork)
        case maybeArtwork of
            Nothing -> errorJson 1 "Failed to parse request body as Artwork"
            Just newArtwork -> do
                newId <- runSql $ insert newArtwork                
                runSql $ P.update artworkId [ArtworkTitle =. artworkTitle newArtwork]
                runSql $ P.update artworkId [ArtworkContent =. artworkContent newArtwork]                                
                json $ object ["result" .= String "success"]          
                
    -- delete
    post ("artwork" <//> var <//> "delete") $ \artworkId -> do
        count <- runSql $ P.count [ArtworkId ==. artworkId]
        case count of
            0 -> errorJson 2 $ pack $ "Could not find an artwork with matching id" ++ (show artworkId)
            _ -> do
                runSql $ P.delete (artworkId :: ArtworkId)
                json $ object ["result" .= String "success"]