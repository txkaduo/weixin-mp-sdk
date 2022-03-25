{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-prof-auto #-}
module WeiXin.PublicPlatform.ThirdParty.Types where

-- {{{1 imports
import           ClassyPrelude
#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Reader                 (asks)
import           Control.DeepSeq       (NFData)
#endif
import           Data.Aeson            as A
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Shakespeare.I18N (ToMessage (..))

import           WeiXin.PublicPlatform.Class
-- }}}1


-- | component_verify_ticket
newtype ComponentVerifyTicket = ComponentVerifyTicket { unComponentVerifyTicket :: Text }
                        deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql
                                 , NFData
                                 , ToMessage, ToMarkup
                                 )

-- | authorization_code
newtype WxppTpAuthCode = WxppTpAuthCode { unWxppTpAuthCode :: Text }
                        deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql
                                 , NFData
                                 , ToMessage, ToMarkup
                                 )

-- | 文档没有说明授权失败时，如何从微信平台返回
-- 暂时认为跟 oauth 一样的逻辑
deniedTpAuthCode :: WxppTpAuthCode -> Bool
deniedTpAuthCode (WxppTpAuthCode x) = null x || x == "authdeny"


-- | refresh token
-- 因为通常跟相应的 AppId 一起使用，所以打包在一起
data WxppTpRefreshToken = WxppTpRefreshToken
  { tpRefreshTokenData   :: Text
  , tpRefreshTokenApp    :: WxppAppID
  -- ^ 授权方的 app id
  }
  deriving (Show, Eq, Typeable, Generic)


-- | component_access_token
-- 因为通常跟相应的 AppId 一起使用，所以打包在一起
data WxppTpAccessToken = WxppTpAccessToken
  { tpAccessTokenData   :: Text
  , tpAccessTokenApp    :: WxppAppID
  -- ^ 我方的 app id
  }
  deriving (Show, Eq, Typeable, Generic)


-- | pre_auth_code
newtype ComponentPreAuthCode = ComponentPreAuthCode { unComponentPreAuthCode :: Text }
                        deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql
                                 , NFData
                                 , ToMessage, ToMarkup
                                 )


-- | 第三方接口多处用到的类似的XML报文中的真正数据
-- 是授权事件接收URL能收到的全部事件
data WxppTpEvent = WxppTpEventVerifyTicket ComponentVerifyTicket
                   -- ^ component_verify_ticket 事件推送
                   | WxppTpEventUnauthorized WxppAppID
                   -- ^ 取消授权通知：数据为授权方的app id
                   | WxppTpEventAuthorized WxppAppID WxppTpAuthCode UTCTime
                   -- ^ 授权成功通知：授权方的app id, 授权码，过期时间
                   | WxppTpEventUpdateAuthorized WxppAppID WxppTpAuthCode UTCTime
                   -- ^ 授权更新通知：授权方的app id, 授权码，过期时间
                   deriving (Show, Eq)

-- | 授权操作接收操作URL上收到的通知报文
data WxppTpEventNotice = WxppTpEventNotice
                            { tpNoticeComponentAppID :: WxppAppID
                            -- ^ 第三方平台的appid，不是授权方的appid
                            , tpNoticeCreateTime     :: UTCTime
                            -- ^ 事件产生时间
                            , tpNoticeEvent          :: WxppTpEvent
                            }
                            deriving (Show)

-- vim: set foldmethod=marker:
