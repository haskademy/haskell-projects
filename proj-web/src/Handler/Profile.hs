{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Profile where

import           Import

import           Yesod.Form.Bootstrap3

import           Proj.Models
import           Proj.Models.Profile

getIndexProfileR :: Handler Html
getIndexProfileR = do
    Entity _ user <- requireAuth
    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")

getEditProfileR :: Handler Html
getEditProfileR = do
    Entity userId _ <- requireAuth
    profile <- maybe notFound pure =<< runDB (selectProfile userId)
    ((_, formWidget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm (Just profile)
    defaultLayout $ do
        setTitle "Edit Profile"
        $(widgetFile "profile/edit")

postEditProfileR :: Handler Html
postEditProfileR = do
    Entity userId _ <- requireAuth
    -- profile <- maybe notFound pure =<< runDB (selectProfile userId)
    ((formResult, formWidget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm Nothing

    case formResult of
        FormSuccess EditProfile {..} -> do
            runDB $ do
                update userId [UserName =. epName]
                if epLearner
                    then void $ insertUnique (Learner userId)
                    else deleteWhere [LearnerUser ==. userId]
                if epMentor
                    then void $ insertUnique (Mentor userId)
                    else deleteWhere [MentorUser ==. userId]
            redirect (ProfileR EditProfileR)
        _ ->
            defaultLayout $ do
                setTitle "Edit Profile"
                $(widgetFile "profile/edit")

data EditProfile = EditProfile
    { epName    :: Text
    , epLearner :: Bool
    , epMentor  :: Bool
    } deriving (Eq, Show)

profileForm :: Maybe Profile -> AForm Handler EditProfile
profileForm mprofile =
    EditProfile
    <$> areq textField
        (bfs (asText "Name: "))
        (fmap (userName . entityVal . profileUser) mprofile)
    <*> areq checkBoxField
        (bfs (asText "Mentor: "))
        (fmap (isJust . profileMentor) mprofile)
    <*> areq checkBoxField
        (bfs (asText "Learner: "))
        (fmap (isJust . profileLearner) mprofile)
