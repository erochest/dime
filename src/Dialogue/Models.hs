{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Dialogue.Models where


import           Control.Arrow
import qualified Data.HashMap.Strict    as M
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.TH

import           Dialogue.Fields


share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

type HandleIndex = M.HashMap T.Text HandleId

getProfile :: (Entity Profile -> Bool) -> [Entity Profile]
           -> Maybe (ProfileId, T.Text)
getProfile p = listToMaybe
             . map (entityKey &&& (_profileNickname . entityVal))
             . filter p
