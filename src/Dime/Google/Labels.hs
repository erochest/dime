{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Labels where


import           Control.Arrow
import           Control.Error
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.Encoding

import           Dime.Google.Network
import           Dime.Google.Types
import           Dime.Types


list :: Google [Label]
list = _labelsLabels <$> (getJSON url :: Google Labels)
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/labels"

get :: LabelId -> Google Label
get lId = getJSON $  "https://www.googleapis.com/gmail/v1/users/me/labels/"
                  <> encodeUtf8 lId

create :: LabelInfo -> Google Label
create lInfo =
    maybe (liftE $ throwE "Invalid label info.") return
        =<< postJSON "https://www.googleapis.com/gmail/v1/users/me/labels"
                     (toJSON lInfo)

ensure :: LabelName -> Google Label
ensure label = do
    labels <- index
    case M.lookup label labels of
         Just lId -> get lId
         Nothing  -> create $ LabelInfo label ShowLabel ShowMessage

index :: Google (M.HashMap T.Text T.Text)
index = M.fromList . map (_labelName &&& _labelId) <$> list
