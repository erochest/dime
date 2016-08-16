{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.Gmail where


import           Control.Arrow
import           Control.Error
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Dime.Google
import           Dime.Google.Types


archiveGmail :: FilePath -> FilePath -> FilePath -> T.Text -> Script ()
archiveGmail configFile _userIndex _archive _label = runGoogle' configFile $ do
    liftIO . TIO.putStrLn . mappend "You are: " =<< getUser
    liftIO . TIO.putStrLn $ "Labels:"
    liftIO . mapM_ (print . (_labelId &&& _labelName)) =<< listLabels
