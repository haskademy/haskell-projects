module Proj.Models.Profile where

import Control.Monad.IO.Class
import Data.Maybe
import Database.Esqueleto

import Proj.Models

selectProfile :: MonadIO m => UserId -> SqlPersistT m (Maybe Profile)
selectProfile userId =
    fmap (fmap conv . listToMaybe) $
    select $
    from $ \(user `LeftOuterJoin` mentor `LeftOuterJoin` learner `LeftOuterJoin` admin) -> do
    on (just (user ^. UserId) ==. mentor ?. MentorUser)
    on (just (user ^. UserId) ==. learner ?. LearnerUser)
    on (just (user ^. UserId) ==. admin ?. AdminUser)
    where_ (user ^. UserId ==. val userId)
    limit 1
    return (user, learner, mentor, admin)
  where
    conv (euser, elearner, ementor, eadmin) =
        Profile euser elearner ementor eadmin
