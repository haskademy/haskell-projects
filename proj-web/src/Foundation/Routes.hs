{-# LANGUAGE OverloadedStrings #-}


module Foundation.Routes where

import Rowdy.Yesod
import Rowdy

routes = toYesod $ do
    get "HomeR"
    "profile" // do
        get "ProfileR"
        "edit" // do
            get "EditProfileR"
            post "EditProfileR"
    "static" // subsite "StaticR" "Static" "appStatic"
    "auth" // subsite "AuthR" "Auth" "getAuth"
    "favicon.ico" // get "FaviconR"
    "robots.txt" // get "RobotsR"


