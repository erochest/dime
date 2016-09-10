{-# LANGUAGE DeriveDataTypeable         #-}
-- {-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DeriveTraversable          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedLists            #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE TemplateHaskell            #-}


module Dialog.Types where


-- import           Control.Lens
import           Data.Data
-- import qualified Data.Text              as T
import           GHC.Generics           hiding (to)


data Service = Twitter
             -- | Google
             -- | IRC
             -- | DoNote
             -- | Journal
             deriving (Eq, Read, Show, Data, Typeable, Generic)

data ExportFormat = ExportJSON | ExportEPUB
                  deriving (Eq, Show, Data, Typeable, Generic)
