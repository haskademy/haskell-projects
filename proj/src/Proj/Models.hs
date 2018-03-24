{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Proj.Models where

import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Database.Persist    (Entity)
import           Database.Persist.TH (mkPersist, persistLowerCase, share,
                                      sqlSettings)

share [mkPersist sqlSettings] [persistLowerCase|

-- user and user management models
User
    name    Text
    created UTCTime
    updated UTCTime

    deriving Eq Show

Admin
    user    UserId
    created UTCTime

    deriving Eq Show

Learner
    user    UserId

    deriving Eq Show

Mentor
    user    UserId

    deriving Eq Show

-- projects

Project
    name        Text
    created     UTCTime
    description Text

    deriving Eq Show

|]

-- | A 'Profile' is not something that exists in the database. It is the
-- result of collecting all of a relevant user's information.
data Profile
    = Profile
    { profileUser    :: Entity User
    , profileLearner :: Entity Learner
    , profileMentor  :: Entity Mentor
    , profileAdmin   :: Maybe (Entity Admin)
    } deriving (Eq, Show)
