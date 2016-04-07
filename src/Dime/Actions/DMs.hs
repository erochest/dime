{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.DMs where


import           Control.Concurrent             (threadDelay)
import           Control.Error
import           Control.Lens                   hiding (from, to, (??))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import qualified Data.List                      as L
import           Network.HTTP.Conduit
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Conduit.Base
import           Web.Twitter.Conduit.Parameters
import           Web.Twitter.Conduit.Request
import           Web.Twitter.Conduit.Types
import           Web.Twitter.Types

import           Dime.Auth
import           Dime.Config


scrapeDMs :: FilePath -> UserName -> FilePath -> Script ()
scrapeDMs configFile _friend output = do
    config <- readConfig configFile
    twInfo <- getTWInfo config ?? "You have to call 'dime login' first."

    manager <- scriptIO $ newManager tlsManagerSettings
    to <- scriptIO . runResourceT $
        walkHistory twInfo manager dmId (directMessages & count ?~ 100) Nothing
    from <- scriptIO . runResourceT $
        walkHistory twInfo manager dmId (directMessagesSent & count ?~ 100)
            Nothing

    scriptIO . BL.writeFile output . encode . L.sortOn dmId $ from ++ to

walkHistory :: ( Monad m
               , MonadResource m
               , FromJSON a
               , HasMaxIdParam (APIRequest apiName [a])
               )
            => TWInfo
            -> Manager
            -> (a -> Integer)
            -> APIRequest apiName [a]
            -> Maybe Integer
            -> m [a]
walkHistory twInfo manager getId apireq mMaxId = do
    liftIO . threadDelay . floor $ (60 * 1e6 :: Double)
    liftIO . putStrLn $ "retrieving with max ID " ++ show mMaxId
    res <- call twInfo manager $ apireq & maxId .~ mMaxId
    case res of
      [] -> return res
      _  -> let nextId = minimum $ fmap getId res
            in  (res ++) <$> walkHistory twInfo manager getId apireq
                                (Just (nextId - 1))
