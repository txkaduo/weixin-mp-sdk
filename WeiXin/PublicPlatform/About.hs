module WeiXin.PublicPlatform.About where

import           ClassyPrelude

import           Yesod.Helpers.About as A

import           Data.Version        (Version)
import           Paths_weixin_mp_sdk (version)


gitRevision :: String
gitRevision = $(A.gitRevision)

gitVersion :: String
gitVersion = $(A.gitVersion)

buildTime :: String
buildTime = $(A.buildTime)

buildUser :: String
buildUser = $(A.buildUser)

libraryVersion :: Version
libraryVersion = version
