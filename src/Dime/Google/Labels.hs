{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Labels where


import           Control.Arrow
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.Encoding

import           Dime.Google.DSL     hiding (get)
import qualified Dime.Google.DSL     as DSL
import           Dime.Google.Types


list :: GoogleAction [Label]
list = _labelsLabels <$> DSL.get url []
    where
        url = "/gmail/v1/users/me/labels"

get :: LabelId -> GoogleAction Label
get lId = DSL.get url []
    where
        url = "/gmail/v1/users/me/labels/" <> encodeUtf8 lId

create :: LabelInfo -> GoogleAction Label
create = post' "/gmail/v1/users/me/labels/" . toJSON

{-
 - create :: LabelInfo -> Google Label
 - create lInfo =
 -     maybe (liftE $ throwE "Invalid label info.") return
 -         =<< postJSON "https://www.googleapis.com/gmail/v1/users/me/labels"
 -                      (toJSON lInfo)
 -}

ensure :: LabelName -> GoogleAction Label
ensure label = do
    labels <- index
    case M.lookup label labels of
         Just lId -> get lId
         Nothing  -> create $ LabelInfo label ShowLabel ShowMessage

index :: GoogleAction (M.HashMap T.Text T.Text)
index = M.fromList . map (_labelName &&& _labelId) <$> list
