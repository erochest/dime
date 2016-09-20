module Dialogue.Handles where


import           Control.Arrow           ((&&&))
import           Data.Bifunctor          (first)
import qualified Data.HashMap.Strict     as M
import qualified Data.Text               as T
import           Database.Persist

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Types.Dialogue


indexHandles :: Service -> Dialogue (M.HashMap T.Text HandleId)
indexHandles s =
    M.fromList . fmap (first _handleHandle . (entityVal &&& entityKey))
        <$> liftSql (selectList [HandleService ==. s] [])
