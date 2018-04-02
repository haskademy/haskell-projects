module Models.Profile where

import Database.Esqueleto

import Models

selectProfile :: UserId -> SqlPersistT m (Maybe Profile)
selectProfile userId =
    select $
    from $ \(user `LeftOuterJoin` mentor `LeftOuterJoin` learner) -> do
    on_ (user ^. UserId ==. mentor ^? MentorUser)
    on_ (user ^. UserId ==. learner ^? LearnerUser)
    where_ (user ^. UserId ==. val userId)
    limit 1
    return (user, mentor, learner

