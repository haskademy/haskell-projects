{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Profile where

import Import

import Proj.Models

getProfileR :: Handler Html
getProfileR = do
    Entity _ user <- requireAuth
    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")

getEditProfileR :: Handler Html
getEditProfileR = do
    defaultLayout $ do
        setTitle "Edit Profile"
        $(widgetFile "profile/edit")

postEditProfileR :: Handler Html
postEditProfileR = do
    redirect EditProfileR
