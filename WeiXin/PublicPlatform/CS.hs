module WeiXin.PublicPlatform.CS where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Error
import qualified Data.HashMap.Strict        as HM


-- | 使用客服接口发送消息给指定用户
-- 只能用于用户主动发消息给我们之后的一段时间内使用（文档说是48小时）
wxppCsSendOutMsg ::
    (MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Maybe WxppKfAccount
    -> WxppOutMsgEntity
            -- ^ 其中的 wxppOutFromUserName 字段是没用到的
    -> m ()
wxppCsSendOutMsg (AccessToken { accessTokenData = atk }) m_kf_account out_msg_entity = do
    let url = "https://api.weixin.qq.com/cgi-bin/message/custom/send"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ toJSON $ obj_for_out_msg_entity out_msg_entity)
        >>= asWxppWsResponseVoid
    where
        obj_for_out_msg_entity e = do
            Object hm <- obj_for_out_msg $ wxppOutMessage e
            return $ Object $
                HM.insert "touser" (toJSON $ unWxppOpenID $ wxppOutToUserName e) $
                    maybe id (\kf_account -> HM.insert "customservice"
                                                (object [ "kf_account" .= unWxppKfAccount kf_account ])
                            ) m_kf_account $
                        hm

        obj_for_out_msg (WxppOutMsgText t) = Just $ object
                                            [ "msgtype" .= ("text" :: Text)
                                            , "text"    .= object [ "content" .= t ]
                                            ]
        obj_for_out_msg (WxppOutMsgImage media_id) = Just $ object
                                            [ "msgtype" .= ("image" :: Text)
                                            , "image"   .= object [ "media_id" .= unWxppBriefMediaID media_id ]
                                            ]
        obj_for_out_msg (WxppOutMsgVoice media_id) = Just $ object
                                            [ "msgtype" .= ("voice" :: Text)
                                            , "voice"   .= object [ "media_id" .= unWxppBriefMediaID media_id ]
                                            ]
        obj_for_out_msg (WxppOutMsgVideo media_id m_thumb_media_id title desc) = Just $
                            object
                                [ "msgtype" .= ("video" :: Text)
                                , "video"   .= object
                                                [ "media_id"        .= unWxppBriefMediaID media_id
                                                , "thumb_media_id"  .= fmap unWxppBriefMediaID m_thumb_media_id
                                                , "title"           .= title
                                                , "description"     .= desc
                                                ]
                                ]
        obj_for_out_msg (WxppOutMsgMusic thumb_media_id title desc url hq_url) = Just $
                            object
                                [ "msgtype" .= ("music" :: Text)
                                , "music"   .= object
                                                [ "thumb_media_id"  .= unWxppBriefMediaID thumb_media_id
                                                , "musicurl"        .= fmap unUrlText url
                                                , "hqmusicurl"      .= fmap unUrlText hq_url
                                                , "title"           .= title
                                                , "description"     .= desc
                                                ]
                                ]
        obj_for_out_msg (WxppOutMsgNews articles) = Just $
                            object
                                [ "msgtype" .= ("news" :: Text)
                                , "news"    .= object
                                                [ "articles"    .= map obj_of_article articles
                                                ]
                                ]

        -- XXX: 文档并没有这种消息在这个接口应如何发
        -- 实用中，应该也不会在客服接口中发这类型的消息
        -- 因为这个这个消息的意义就在用回复，而不是独立地发送给用户
        obj_for_out_msg (WxppOutMsgTransferToCustomerService {}) = Nothing

        obj_of_article article =
                    object
                        [ "title"       .= wxppArticleTitle article
                        , "description" .= wxppArticleDesc article
                        , "url"         .= fmap unUrlText (wxppArticleUrl article)
                        , "picurl"      .= fmap unUrlText (wxppArticlePicUrl article)
                        ]
