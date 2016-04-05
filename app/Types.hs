module Types where


import           Web.Twitter.Types

-- import           Dime.Types


data Actions
    = Login { loginConfig :: !FilePath
            }
    | DMs { dmsConfig :: !FilePath
          , dmsFriend :: !UserName
          , dmsOutput :: !FilePath
          }
             deriving (Show, Eq)
