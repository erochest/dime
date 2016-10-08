{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dialogue.Types.Publish where


import           Control.Lens
import           Data.Data
import qualified Data.HashMap.Strict  as M
import           Data.Int
import qualified Data.Sequence        as Seq
import qualified Data.Text            as T
import           Data.Text.Format
import qualified Data.Text.Lazy       as TL
import           Data.Time
import           Database.Persist.Sql
import           GHC.Generics

import           Dialogue.Fields
import           Dialogue.Models


type HandleNames = M.HashMap Int64 T.Text

blockSender :: HandleNames -> HandleId -> T.Text
blockSender hs pid =
    M.lookupDefault (TL.toStrict . format "?{}?" $ Only s) s hs
    where
        s = fromSqlKey pid

data PublishBlock
    = PublishBlock
    { _pbName    :: !T.Text
    , _pbPrimary :: !Bool
    , _pbService :: !Service
    , _pbDate    :: !UTCTime
    , _pbContent :: !T.Text
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''PublishBlock)

data BlockGroup
    = BlockGroup
    { _bgStartTime :: !UTCTime
    , _bgEndTime   :: !UTCTime
    , _bgBlocks    :: !(Seq.Seq PublishBlock)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''BlockGroup)

class Publishable a where
    toBlock :: HandleNames -> T.Text -> a -> PublishBlock

block :: HandleNames -> HandleId -> T.Text -> Service -> UTCTime -> T.Text
      -> PublishBlock
block handles hid name =
    PublishBlock sender primary
    where
        sender  = blockSender handles hid
        primary = sender == name

instance Publishable AdiumMessage where
    toBlock handles n AdiumMessage{ _adiumMessageCreatedAt=d
                                  , _adiumMessageContent=c
                                  , _adiumMessageSenderHandleId=s} =
        block handles s n AdiumService d c

instance Publishable GoogleMessage where
    toBlock handles n GoogleMessage{ _googleMessageCreatedAt=d
                                   , _googleMessageContent=c
                                   , _googleMessageSenderId=s} =
        block handles s n GoogleService d c

instance Publishable Journal where
    toBlock _ n Journal{_journalDate=d, _journalContent=c} =
        PublishBlock n True JournalService d c

instance Publishable NoteMessage where
    toBlock _ n NoteMessage{_noteMessageCreatedAt=d, _noteMessageContent=c} =
        PublishBlock n True NoteService d c

instance Publishable TwitterMessage where
    toBlock handles n TwitterMessage{ _twitterMessageCreatedAt=d
                                    , _twitterMessageContent=c
                                    , _twitterMessageSenderHandleId=s } =
        block handles s n TwitterService d c
