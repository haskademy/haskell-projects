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

getProfileR :: Handler Html
getProfileR = do
    Entity _ user <- requireAuth
    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")

getEditProfileR :: Handler Html
getEditProfileR = do
    _ <- requireAuth
    ((_, formWidget), encType) <- runFormPost $ renderBootstrap3 BootstrapBasicForm profileForm
    defaultLayout $ do
        setTitle "Edit Profile"
        $(widgetFile "profile/edit")

postEditProfileR :: Handler Html
postEditProfileR = do
    redirect EditProfileR

data EditProfile = EditProfile
    { epName    :: Text
    , epLearner :: Bool
    , epMentor  :: Bool
    } deriving (Eq, Show)

profileForm :: Profile -> AForm Handler EditProfile
profileForm Profile{..} =
    EditProfile
    <$> areq textField (bfs (asText "Name: ")) (Just (userName (entityVal profileUser)))
    <*> areq checkBoxField (bfs (asText "Mentor: ")) (Just (isJust (profileMentor)))
    <*> areq checkBoxField (bfs (asText "Learner: "))  (Just (isJust (profileLearner)))
