{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}


module Dialogue.Types.Stats where


import           Data.Aeson
import           Data.Aeson.Types    (Options)
import           Data.Data
import qualified Data.HashMap.Strict as M
import           Data.Int
import qualified Data.Text           as T
import           GHC.Generics

import           Dialogue.Utils


type MonthKey       = (Integer, Int)
type ProfileIndex a = M.HashMap Int64    a
type MonthIndex   a = M.HashMap MonthKey a

statsOptions :: Options
statsOptions = prefixOptions 2

data ProfileCount
    = ProfileCount
    { pcId    :: !T.Text
    , pcCount :: !Int
    } deriving (Show, Eq, Typeable, Generic)

instance ToJSON ProfileCount where
    toJSON     = genericToJSON     statsOptions
    toEncoding = genericToEncoding statsOptions

instance FromJSON ProfileCount where
    parseJSON = genericParseJSON statsOptions

data ServiceCount
    = ServiceCount
    { scPrimary   :: !ProfileCount
    , scSecondary :: !ProfileCount
    } deriving (Show, Eq, Typeable, Generic)

instance ToJSON ServiceCount where
    toJSON     = genericToJSON     statsOptions
    toEncoding = genericToEncoding statsOptions

instance FromJSON ServiceCount where
    parseJSON = genericParseJSON statsOptions

data MonthCount
    = MonthCount
    { mcYear    :: !Integer
    , mcMonth   :: !Int
    , mcAdium   :: !ServiceCount
    , mcGoogle  :: !ServiceCount
    , mcTwitter :: !ServiceCount
    } deriving (Show, Eq, Typeable, Generic)

instance ToJSON MonthCount where
    toJSON     = genericToJSON     statsOptions
    toEncoding = genericToEncoding statsOptions

instance FromJSON MonthCount where
    parseJSON = genericParseJSON statsOptions
