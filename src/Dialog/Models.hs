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


module Dialog.Models where


import           Data.Text              (Text)
import           Database.Persist.Quasi
import           Database.Persist.TH


share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
    $(persistFileWith lowerCaseSettings "config/models")
