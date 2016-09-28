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


import           Data.HashMap.Strict    (HashMap)
import           Data.Text              (Text)
import           Data.Time
import           Database.Persist.Quasi
import           Database.Persist.TH

import           Dialogue.Fields


share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

type HandleIndex = HashMap Text HandleId
