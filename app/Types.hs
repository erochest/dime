module Types where


data Actions
    = Login { loginConfig :: !FilePath
            }
    | DMs { dmsConfig :: !FilePath
          , dmsOutput :: !FilePath
          }
             deriving (Show, Eq)
