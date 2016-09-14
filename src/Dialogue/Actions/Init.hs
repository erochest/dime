{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}


module Dialogue.Actions.Init where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.Persist

import           Dialogue.Models
import           Dialogue.Types
import           Dialogue.Utils


initialize :: FilePath -> Script ()
initialize dbFile = runDialogueS' (T.pack dbFile) $
    liftSql . mapM_ insert
        =<< (  unfoldM (promptMaybe "New profile")
            :: Dialogue SomeException [Profile])

init' :: (Exception e, MessageStream ms) => Proxy ms -> Dialogue e ()
init' ps = do
    let name = streamName' ps

    liftIO $ TIO.putStrLn name
    s <- initStream ps
    case s of
        Just s' -> do
            activate s'
            liftIO . TIO.putStrLn $ name <> " activated."
        Nothing -> liftIO . TIO.putStrLn $ name <> " not activated."
