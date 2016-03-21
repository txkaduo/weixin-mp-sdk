module WeiXin.PublicPlatform.CS where

import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Reader                 (asks)
import Data.Aeson

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import qualified Data.HashMap.Strict        as HM


-- | 使用客服接口发送消息给指定用户
-- 只能用于用户主动发消息给我们之后的一段时间内使用（文档说是48小时）
wxppCsSendOutMsg2 :: (WxppApiMonad env m)
                  => AccessToken
                  -> Maybe WxppKfAccount
                  -> WxppOpenID
                  -> WxppOutMsg
                          -- ^ 其中的 wxppOutFromUserName 字段是没用到的
                  -> m ()
wxppCsSendOutMsg2 (AccessToken { accessTokenData = atk }) m_kf_account to_open_id out_msg = do
    let url = wxppRemoteApiBaseUrl <> "/message/custom/send"
        opts = defaults & param "access_token" .~ [ atk ]

    sess <- asks getWreqSession
    liftIO (WS.postWith opts sess url $ toJSON $ mk_out_obj)
        >>= asWxppWsResponseVoid
    where
        mk_out_obj = do
            Object hm <- obj_for_out_msg out_msg
            return $ Object $
                HM.insert "touser" (toJSON $ unWxppOpenID $ to_open_id) $
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
                                            , "image"   .= object [ "media_id" .= media_id ]
                                            ]
        obj_for_out_msg (WxppOutMsgVoice media_id) = Just $ object
                                            [ "msgtype" .= ("voice" :: Text)
                                            , "voice"   .= object [ "media_id" .= media_id ]
                                            ]
        obj_for_out_msg (WxppOutMsgVideo media_id m_thumb_media_id title desc) = Just $
                            object
                                [ "msgtype" .= ("video" :: Text)
                                , "video"   .= object
                                                [ "media_id"        .= media_id
                                                , "thumb_media_id"  .= m_thumb_media_id
                                                , "title"           .= title
                                                , "description"     .= desc
                                                ]
                                ]
        obj_for_out_msg (WxppOutMsgMusic thumb_media_id title desc url hq_url) = Just $
                            object
                                [ "msgtype" .= ("music" :: Text)
                                , "music"   .= object
                                                [ "thumb_media_id"  .= thumb_media_id
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


{-# DEPRECATED wxppCsSendOutMsg "use wxppCsSendOutMsg2 instead" #-}
wxppCsSendOutMsg :: (WxppApiMonad env m)
                 => AccessToken
                 -> Maybe WxppKfAccount
                 -> WxppOutMsgEntity
                         -- ^ 其中的 wxppOutFromUserName, wxppOutCreatedTime 字段是没用到的
                 -> m ()
wxppCsSendOutMsg atk m_kf_account out_msg_entity = do
    wxppCsSendOutMsg2 atk m_kf_account to_open_id out_msg
    where
        to_open_id = wxppOutToUserName out_msg_entity
        out_msg = wxppOutMessage out_msg_entity
