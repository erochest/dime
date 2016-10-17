{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Dialogue.Actions.Init where


import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Database.Persist

import           Dialogue.Models
import           Dialogue.Streams
import           Dialogue.Streams.Adium
import           Dialogue.Streams.GDoc
import           Dialogue.Streams.Google
import           Dialogue.Streams.Mail
import           Dialogue.Streams.Note
import           Dialogue.Streams.Twitter
import           Dialogue.Types
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


initialize :: FilePath -> Script ()
initialize dbFile = runDialogueS' (T.pack dbFile) $ do
    liftSql . mapM_ insert
        =<< (unfoldM (promptMaybe "New profile") :: Dialogue [Profile])
    void $ init' (Proxy :: Proxy AdiumStream)
    void $ init' (Proxy :: Proxy GoogleStream)
    void $ init' (Proxy :: Proxy GDocStream)
    void $ init' (Proxy :: Proxy MailStream)
    void $ init' (Proxy :: Proxy NoteStream)
    void $ init' (Proxy :: Proxy TwitterStream)

init' :: (Promptable ms, MessageStream ms mi, HasServiceId ms)
      => Proxy ms -> Dialogue (Maybe ms)
init' ps = do
    a <- isActive' ps
    if not a
        then do
            let name = streamName' ps

            liftIO $ TIO.putStrLn name
            s <- promptMaybe name
            case s of
                Just s' -> do
                    void $ activate s'
                    liftIO . TIO.putStrLn $ name <> " activated."
                Nothing -> liftIO . TIO.putStrLn $ name <> " not activated."
            return s
        else
            Just <$> openStream
