{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}


module Dime.Google.DSL where


import           Control.Monad.Free
import           Data.Aeson
import           Network.OAuth.OAuth2
import           Network.Wreq.Types


import           Dime.Google.Network
import           Dime.Google.Types


singleActions :: GoogleAction n -> Google n
singleActions (Pure r) = return r
singleActions (Free (GGet  u ps   k)) = getJSON'  u ps   >>= singleActions . k
singleActions (Free (GPost u ps p k)) = postJSON' u ps p >>= singleActions . k

-- TODO: batchActions
batchActions :: GoogleAction n -> Google n
batchActions = undefined

get :: FromJSON n => URI -> [GetParam] -> GoogleAction n
get u ps = liftF $ GGet u ps id

post' :: (Postable p, FromJSON n) => URI -> p -> GoogleAction n
post' url = post url []

post :: (Postable p, FromJSON n) => URI -> [GetParam] -> p -> GoogleAction n
post u ps p = liftF $ GPost u ps p id
