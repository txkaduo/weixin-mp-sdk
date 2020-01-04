module WeiXin.PublicPlatform.EndUser.Tag
    ( wxppCreateUserTag
    , WxppUserTagInfo(..), wxppListUserTags
    , wxppRenameUserTag
    , wxppDeleteUserTag
    , GetUsersOfTagResult(..), wxppGetUsersOfTag
    , wxppTagUsers, wxppUntagUsers
    , wxppGetTagsOfUser
    ) where

-- {{{1 imports
import ClassyPrelude hiding ((\\))
import Network.Wreq hiding (Proxy)
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Control.Monad.Reader                 (asks)
import Data.Aeson
import qualified Data.Aeson.Extra           as AE
import Data.Proxy

import Data.Conduit

import Yesod.Helpers.Utils                  (nullToNothing)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS

import Yesod.Compat
-- }}}1


data CreateTagResult = CreateTagResult WxppUserTagID Text

instance FromJSON CreateTagResult where
    parseJSON = withObject "CreateTagResult" $ \o -> do
                    o2 <- o .: "tag"
                    CreateTagResult <$> o2 .: "id"
                                    <*> o2 .: "name"

-- | 创建标签
wxppCreateUserTag :: (WxppApiMonad env m)
                  => AccessToken
                  -> Text
                  -> m WxppUserTagID
wxppCreateUserTag (AccessToken { accessTokenData = atk }) name = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/create"
        opts = defaults & param "access_token" .~ [ atk ]

    CreateTagResult tag_id name' <-
        liftIO (WS.postWith opts sess url $ object [ "tag" .= object [ "name" .= name ] ])
                >>= asWxppWsResponseNormal'
    when (name' /= name) $ do
        $logErrorS wxppLogSource $ "user tag created but got different name: "
                    <> "expecting " <> name
                    <> ", but got " <> name'
        liftIO $ throwIO $ userError "unexpected tag name returned"

    return tag_id


data WxppUserTagInfo = WxppUserTagInfo
                        WxppUserTagID
                        Text
                        Int

-- {{{1 instances
instance FromJSON WxppUserTagInfo where
    parseJSON = withObject "WxppUserTagInfo" $ \o ->
                    WxppUserTagInfo <$> o .: "id"
                                    <*> o .: "name"
                                    <*> o .: "count"

instance ToJSON WxppUserTagInfo where
    toJSON (WxppUserTagInfo tag_id name cnt) = object
                                                  [ "id"      .= tag_id
                                                  , "name"    .= name
                                                  , "count"   .= cnt
                                                  ]
-- }}}1


data ListTagResult = ListTagResult { unListTagResult :: [WxppUserTagInfo] }

instance FromJSON ListTagResult where
    parseJSON = withObject "ListTagResult" $ \o ->
                    ListTagResult <$> o .: "tags"


-- | 获取公众号已创建的标签
wxppListUserTags :: (WxppApiMonad env m) => AccessToken -> m [WxppUserTagInfo]
-- {{{1
wxppListUserTags (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/get"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url)
                >>= asWxppWsResponseNormal'
                >>= return . unListTagResult
-- }}}1


-- | 编辑标签
wxppRenameUserTag :: (WxppApiMonad env m)
                  => AccessToken
                  -> WxppUserTagID
                  -> Text
                  -> m ()
-- {{{1
wxppRenameUserTag (AccessToken { accessTokenData = atk }) tag_id name = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/update"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "tag" .= object [ "id" .= tag_id, "name" .= name ] ])
            >>= asWxppWsResponseVoid
-- }}}1


-- | 删除标签
-- 请注意，当某个标签下的粉丝超过10w时，后台不可直接删除标签。此时，开发者可以对该标签下的openid列表，先进行取消标签的操作，直到粉丝数不超过10w后，才可直接删除该标签。
wxppDeleteUserTag :: (WxppApiMonad env m)
                  => AccessToken
                  -> WxppUserTagID
                  -> m ()
-- {{{1
wxppDeleteUserTag (AccessToken { accessTokenData = atk }) tag_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/delete"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "tag" .= object [ "id" .= tag_id ] ])
            >>= asWxppWsResponseVoid
-- }}}1


data GetUsersOfTagResult = GetUsersOfTagResult
                            Int             -- ^ count
                            [WxppOpenID]
                            (Maybe WxppOpenID) -- ^ next open id


-- {{{1 instances
instance FromJSON GetUsersOfTagResult where
    parseJSON = withObject "GetUsersOfTagResult" $ \obj -> do
                    count <- obj .: "count"

                    -- 当没数据时，平台不知道会不会发出 data 字段
                    m_data_obj <- obj .:? "data"
                    lst <- case m_data_obj of
                            Nothing -> return []
                            Just o  -> map WxppOpenID <$> o .: "openid"

                    -- 平台是用空字串表示结束的
                    next_openid <- fmap WxppOpenID . join . fmap nullToNothing <$> obj .:? "next_openid"
                    return $ GetUsersOfTagResult count lst next_openid

instance HasWxppOpenIDList GetUsersOfTagResult where
  getWxppOpenIDList (GetUsersOfTagResult _ x _) = x
-- }}}1


-- | 获取标签下粉丝列表
wxppGetUsersOfTag :: (WxppApiMonad env m)
                  => m AccessToken
                  -- ^ 我们要反复取用 access token,　而且不确定用多长时间
                  -> WxppUserTagID
                  -> SourceC m GetUsersOfTagResult
-- {{{1
wxppGetUsersOfTag get_atk tag_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)

    let url         = wxppUrlConfSecureApiBase url_conf <> "/user/tag/get"
        loop m_start_id = do
            AccessToken { accessTokenData = atk } <- lift get_atk
            let opts = defaults & param "access_token" .~ [ atk ]
                                & param "tagid" .~ [ tshow (unWxppUserTagID tag_id) ]
                                & (case m_start_id of
                                    Nothing     -> id
                                    Just (WxppOpenID start_open_id) ->
                                        param "next_openid" .~ [ start_open_id ]
                                    )

            r@(GetUsersOfTagResult _ _ m_next_id) <-
                liftIO (WS.getWith opts sess url) >>= asWxppWsResponseNormal'
            yield r

            maybe (return ()) (loop . Just) m_next_id

    loop Nothing
-- }}}1


-- | 批量为用户打标签
-- 每次传入的openid列表个数不能超过50个
wxppTagUsers :: (WxppApiMonad env m)
             => AccessToken
             -> WxppUserTagID
             -> [WxppOpenID]
             -> m ()
-- {{{1
wxppTagUsers (AccessToken { accessTokenData = atk }) tag_id open_ids = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/members/batchtagging"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "tagid" .= tag_id, "openid_list" .= open_ids ])
            >>= asWxppWsResponseVoid
-- }}}1


-- | 批量为用户取消标签
-- 每次传入的openid列表个数不能超过50个
wxppUntagUsers :: (WxppApiMonad env m)
               => AccessToken
               -> WxppUserTagID
               -> [WxppOpenID]
               -> m ()
-- {{{1
wxppUntagUsers (AccessToken { accessTokenData = atk }) tag_id open_ids = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/members/batchuntagging"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "tagid" .= tag_id, "openid_list" .= open_ids ])
            >>= asWxppWsResponseVoid
-- }}}1


-- | 获取用户身上的标签列表
wxppGetTagsOfUser :: (WxppApiMonad env m)
                  => AccessToken
                  -> WxppOpenID
                  -> m [WxppUserTagID]
-- {{{1
wxppGetTagsOfUser (AccessToken { accessTokenData = atk }) open_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/tags/getidlist"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "openid" .= open_id ])
            >>= asWxppWsResponseNormal'
            >>= return . AE.getSingObject (Proxy :: Proxy "tagid_list")
-- }}}1


-- vim: set foldmethod=marker:
