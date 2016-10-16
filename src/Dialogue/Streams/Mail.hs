{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dialogue.Streams.Mail where


import           Control.Arrow
import           Control.Exception.Safe
import           Control.Lens
import           Data.Data
import qualified Data.Text               as T
import           Database.Persist
import           GHC.Generics

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Types.Dialogue


newtype MailException = MailException { unMailException :: T.Text }
                      deriving (Show, Eq, Data, Typeable, Generic)

instance Exception MailException

data MailStream
    = MailStream
    { _mailServiceId :: !(Maybe ServiceInfoId)
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''MailStream)
$(makeFields ''MailStream)

loadMail :: Dialogue MailStream
loadMail = do
    s <- liftSql
      $  selectFirst [ ServiceInfoService  ==. MailService
                     , ServiceInfoIsActive ==. True
                     ] []
    case s of
        Just (Entity sid _) -> return $ MailStream $ Just sid
        Nothing             -> throwD $ MailException "Invalid service: Mail"

saveMail :: MailStream -> Dialogue (Key ServiceInfo, MailStream)
saveMail ms@(MailStream (Just sid)) = return (sid, ms)
saveMail    (MailStream Nothing) =
    liftSql
        $   (id &&& (MailStream . Just))
        <$> insert (ServiceInfo MailService False Nothing Nothing)

loadMbox :: MailStream -> Dialogue [Entity MailMessage]
loadMbox = const $ return []

loadMailMessages :: Dialogue [Entity MailMessage]
loadMailMessages = liftSql $ selectList [] [Asc MailMessageCreatedAt]
