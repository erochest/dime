module Types where


data Actions
    = Login { loginConfig :: !FilePath
            }
    | DMs { dmsConfig   :: !FilePath
          , dmsOutput   :: !FilePath
          , dmsStateDir :: !FilePath
          }
             deriving (Show, Eq)
