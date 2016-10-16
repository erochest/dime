{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dialogue.Types where


import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Char                (isSpace)
import           Data.Data
import qualified Data.List                as L
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Database.Persist.Sqlite
import           GHC.Generics             hiding (to)
import           Lucid                    (Html)
import           System.Directory
import           System.FilePath
import           System.IO                (hFlush, stdout)

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Streams
import           Dialogue.Streams.Adium
import           Dialogue.Streams.Google
import           Dialogue.Streams.Mail
import           Dialogue.Streams.Note
import           Dialogue.Streams.Twitter
import           Dialogue.Types.Dialogue


-- * Exceptions

newtype ProfileException = ProfileException { unProfileException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ProfileException

newtype ServiceException = ServiceException { unServiceException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ServiceException

-- * Streams

class Publishable b where
    toHTML :: b -> Html ()

data StreamList where
    MkStreamList :: MessageStream ms o => [ms] -> StreamList

data ProxyStreamList where
    MkProxyList :: MessageStream ms o => [Proxy ms] -> ProxyStreamList

-- * Services

data ExportFormat = ExportJSON | ExportEPUB
                  deriving (Eq, Show, Data, Typeable, Generic)

-- * Inputs

data TextInput
    = FileInput !FilePath
    | RawInput !T.Text
    | StdInput
    deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassyPrisms ''TextInput)

-- * Promptable

class Promptable p where
    prompt :: T.Text -> Dialogue p
    promptMaybe :: T.Text -> Dialogue (Maybe p)

    promptMaybe = fmap Just . prompt

checkPrompt :: Promptable p => T.Text -> Dialogue (Maybe p)
checkPrompt msg = do
    add <- prompt msg
    if add then Just <$> prompt "" else return Nothing

profilesFor :: Service -> Dialogue ()
profilesFor s = liftSql (selectList [] []) >>= mapM_ go
    where
        go e@(Entity pid p) = do
            name' <- promptMaybe $  "Handle for " <> (p ^. profileNickname)
            case name' of
                Just name -> liftSql (insert_ $ Handle name s pid) >> go e
                Nothing   -> return ()

instance Promptable T.Text where
    prompt msg =  liftIO
               $  TIO.putStr (msg <> "? ")
               >> hFlush stdout
               >> T.strip <$> TIO.getLine

    promptMaybe = checkPrompt

instance Promptable String where
    prompt msg =   liftIO
               $   TIO.putStr (msg <> "? ")
               >>  hFlush stdout
               >>  L.dropWhile isSpace . L.dropWhileEnd isSpace
               <$> getLine

    promptMaybe = checkPrompt

instance Promptable Bool where
    prompt msg = liftIO $ do
        TIO.putStr (msg <> " [Y/n]? ") >> hFlush stdout
        reply <- T.toLower . T.strip <$> TIO.getLine
        return $ case T.uncons reply of
                    Nothing       -> True
                    Just ('y', _) -> True
                    Just _        -> False

instance Promptable Profile where
    prompt _msg =
        Profile <$> prompt "name" <*> prompt "nickname" <*> prompt "primary"

    promptMaybe = checkPrompt

instance Promptable AdiumStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        profilesFor AdiumService
        AdiumStream Nothing <$> prompt "Adium directory"

    promptMaybe = checkPrompt

instance Promptable MailStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        profilesFor MailService
        return $ MailStream Nothing

    promptMaybe = checkPrompt

instance Promptable NoteStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        defDir <- (</> "Dropbox" </> "DO Note") <$> liftIO getHomeDirectory
        exists <- liftIO $ doesDirectoryExist defDir
        useDef <- if exists
            then prompt $  "Use the default directory ("
                        <> T.pack defDir <> ")"
            else return False
        noteDir <- if useDef
            then return defDir
            else prompt "Note directory"
        return $ NoteStream Nothing noteDir

    promptMaybe = checkPrompt

instance Promptable TwitterStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        (token, secret) <- loginTwitter
        profilesFor TwitterService
        return $ TwitterStream Nothing (token, secret)

    promptMaybe = checkPrompt

instance Promptable GoogleStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        token <- loginGoogle
        profilesFor GoogleService
        return $ GoogleStream Nothing token

    promptMaybe = checkPrompt
