{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}


module Dialogue.Actions.Init where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Dialogue.Types

import           Development.Placeholders


initialize :: FilePath -> Script ()
initialize dbFile = runDialogueS' (T.pack dbFile) $
    $(todo "Create a stream or two.")

runDialogueS' :: T.Text -> Dialogue SomeException a -> Script a
runDialogueS' = runDialogueS

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
