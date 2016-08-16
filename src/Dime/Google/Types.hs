{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dime.Google.Types where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import qualified Data.Text        as T
import           GHC.Generics


data MessageListVisibility = ShowMessage | HideMessage
                           deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''MessageListVisibility)

instance ToJSON MessageListVisibility where
    toJSON ShowMessage = String "show"
    toJSON HideMessage = String "hide"

instance FromJSON MessageListVisibility where
    parseJSON (String "show") = return ShowMessage
    parseJSON (String "hide") = return HideMessage
    parseJSON _               = mzero

data LabelListVisibility = ShowLabel | HideLabel | ShowIfUnread
                         deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''LabelListVisibility)

instance ToJSON LabelListVisibility where
    toJSON ShowLabel    = String "labelShow"
    toJSON HideLabel    = String "labelHide"
    toJSON ShowIfUnread = String "labelShowIfUnread"

instance FromJSON LabelListVisibility where
    parseJSON (String "labelShow")         = return ShowLabel
    parseJSON (String "labelHide")         = return HideLabel
    parseJSON (String "labelShowIfUnread") = return ShowIfUnread
    parseJSON _                            = mzero

data LabelType = System | User
               deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''LabelType)

instance ToJSON LabelType where
    toJSON System = String "system"
    toJSON User   = String "user"

instance FromJSON LabelType where
    parseJSON (String "system") = return System
    parseJSON (String "user")   = return User
    parseJSON _                 = mzero

type LabelId = T.Text

data Label
    = Label
    { _labelId                    :: !LabelId
    , _labelName                  :: !T.Text
    , _labelMessageListVisibility :: !(Maybe MessageListVisibility)
    , _labelLabelListVisibility   :: !(Maybe LabelListVisibility)
    , _labelType                  :: !LabelType
    , _labelMessagesTotal         :: !(Maybe Int)
    , _labelMessagesUnread        :: !(Maybe Int)
    , _labelThreadsTotal          :: !(Maybe Int)
    , _labelThreadsUnread         :: !(Maybe Int)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Label)

labelOptions :: Int -> Options
labelOptions n = defaultOptions
               { fieldLabelModifier = lowerFirst . drop n
               }
    where
        lowerFirst []     = []
        lowerFirst (x:xs) = toLower x : xs

instance ToJSON Label where
    toJSON     = genericToJSON     (labelOptions 6)
    toEncoding = genericToEncoding (labelOptions 6)

instance FromJSON Label where
    parseJSON = genericParseJSON (labelOptions 6)

data Labels
    = Labels
    { _labelsLabels :: [Label]
    } deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON Labels where
    toJSON     = genericToJSON     (labelOptions 7)
    toEncoding = genericToEncoding (labelOptions 7)

instance FromJSON Labels where
    parseJSON = genericParseJSON (labelOptions 7)
