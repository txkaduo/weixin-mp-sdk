module WeiXin.PublicPlatform.Acid
    ( module WeiXin.PublicPlatform.Acid
    , AcidState
    ) where

import ClassyPrelude
import qualified Data.Set                   as Set
import Control.Lens
import Data.SafeCopy
import Data.Acid
import Data.Default                         (Default(..))
import Control.Monad.Reader                 (asks)
import Control.Monad.State                  (modify)
import Control.Monad.Trans.Maybe            (MaybeT(..))
import Text.Shakespeare.I18N                (Lang)
import qualified Data.Map.Strict            as Map

import Yesod.Helpers.SafeCopy
import WeiXin.PublicPlatform.Class

-- | a helper used with 'WxppAcidGetAcccessTokens'
newestUsableAccessToken :: UTCTime -> [(AccessToken, UTCTime)] -> Maybe (AccessToken, UTCTime)
newestUsableAccessToken now lst =
    listToMaybe $ sortBy (comparing $ Down . snd) $ filter ((> now) . snd) $ lst


-- | 服务器进程需持久保持的数据
data WxppAcidState = WxppAcidState {
                    _wxppAcidStateAccessTokens :: ![(AccessToken, UTCTime)]
                        -- ^ access token 更新允许有个过渡期
                        -- 因此记录了最近使用过的所有 AccessToken 及其过期时间
                        -- 这个列表实现时没有明确地限制长度
                        -- 而是定时丢弃过期的 access token
                        -- 注意：这个列表是排了序的

                    , _wxppAcidStateOAuthAccessTokens :: !(Map
                                                            (WxppOpenID, WxppAppID)
                                                            (Set OAuthTokenInfo)
                                                            )
                        -- ^ oauth access tokens 

                    , _wxppAcidStateCachedSnsUserInfo :: !(Map
                                                            ((WxppOpenID, WxppAppID), Lang)
                                                            (TimeTagged OAuthGetUserInfoResult)
                                                            )

                    , _wxppAcidStateJsTickets   :: !(Map WxppAppID (WxppJsTicket, UTCTime))

                    , _wxppAcidStateUploadedMedia :: !(Map WxppAppID (Map SHA256Hash UploadResult))
                    , _wxppAcidStateCachedUserInfo :: !(Map (WxppOpenID, WxppAppID)
                                                            (TimeTagged EndUserQueryResult))
                        -- ^ 把用户的 EndUserQueryResult 缓存一下，以加快消息处理
                    }
                    deriving (Typeable)

$(makeLenses ''WxppAcidState)
$(deriveSafeCopy 2 'base ''WxppAcidState)

instance Default WxppAcidState where
    def = WxppAcidState def def def def def def

wxppAcidGetAcccessTokens :: WxppAppID -> Query WxppAcidState [(AccessToken, UTCTime)]
wxppAcidGetAcccessTokens app_id =
    asks $ (filter $ (== app_id) . accessTokenApp . fst) . _wxppAcidStateAccessTokens

wxppAcidGetAcccessToken :: WxppAppID -> Query WxppAcidState (Maybe (AccessToken, UTCTime))
wxppAcidGetAcccessToken app_id =
    asks $ listToMaybe . (filter $ (== app_id) . accessTokenApp . fst) . _wxppAcidStateAccessTokens

wxppAcidAddAcccessToken ::
    AccessToken
    -> UTCTime
    -> Update WxppAcidState ()
wxppAcidAddAcccessToken atk expiry = do
    modify $ over wxppAcidStateAccessTokens $ \lst ->
        sortBy (comparing $ Down . snd) $ (atk, expiry) : lst

wxppAcidPurgeAcccessToken ::
    UTCTime -> Update WxppAcidState ()
wxppAcidPurgeAcccessToken expiry = do
    modify $ over wxppAcidStateAccessTokens $
                filter ((> expiry) . snd)

wxppAcidAddOAuthAcccessToken :: OAuthAccessTokenPkg
                            -> UTCTime
                            -> Update WxppAcidState ()
wxppAcidAddOAuthAcccessToken atk_p expiry = do
    modify $ over wxppAcidStateOAuthAccessTokens $
        Map.unionWith Set.union $ Map.singleton (open_id, app_id) (Set.singleton info)
    where
        app_id    = oauthAtkPAppID atk_p
        open_id   = oauthAtkPOpenID atk_p
        atk       = oauthAtkPRaw atk_p
        rtk       = oauthAtkPRtk atk_p
        scopes    = oauthAtkPScopes atk_p
        state     = oauthAtkPState atk_p
        info      = OAuthTokenInfo atk rtk scopes state expiry

wxppAcidPurgeOAuthAccessToken :: UTCTime
                                -> Update WxppAcidState ()
wxppAcidPurgeOAuthAccessToken expiry = do
    modify $ over wxppAcidStateOAuthAccessTokens $
        Map.filter (not . Set.null) .
            Map.map (Set.filter ((> expiry) . get_expiry))
    where
        get_expiry (OAuthTokenInfo _ _ _ _ e) = e

wxppAcidLookupOAuthAccessTokens :: WxppAppID
                                -> WxppOpenID
                                -> Query WxppAcidState
                                        (Set OAuthTokenInfo)
wxppAcidLookupOAuthAccessTokens app_id open_id = do
    liftM (fromMaybe Set.empty) $
        asks $ Map.lookup (open_id, app_id) . _wxppAcidStateOAuthAccessTokens

wxppAcidLookupUploadedMediaIDByHash ::
    WxppAppID -> SHA256Hash -> Query WxppAcidState (Maybe UploadResult)
wxppAcidLookupUploadedMediaIDByHash app_id h =
    asks $ preview $ wxppAcidStateUploadedMedia . at app_id . _Just . at h . _Just

wxppAcidSaveUploadedMediaID ::
    WxppAppID
    -> SHA256Hash
    -> UploadResult
    -> Update WxppAcidState ()
wxppAcidSaveUploadedMediaID app_id h u_res =
    modify $ over wxppAcidStateUploadedMedia $
        Map.unionWith Map.union (Map.singleton app_id (Map.singleton h u_res))

wxppAcidAddJsTicket :: WxppAppID
                    -> WxppJsTicket
                    -> UTCTime
                    -> Update WxppAcidState ()
wxppAcidAddJsTicket app_id ticket expiry = do
    modify $ over wxppAcidStateJsTickets $
            Map.union $ Map.singleton app_id (ticket, expiry)

wxppAcidGetJsTicket :: WxppAppID
                    -> Query WxppAcidState (Maybe (WxppJsTicket, UTCTime))
wxppAcidGetJsTicket app_id = do
    asks $ Map.lookup app_id . _wxppAcidStateJsTickets

wxppAcidPurgeJsTicket :: UTCTime
                      -> Update WxppAcidState ()
wxppAcidPurgeJsTicket expiry = do
    modify $ over wxppAcidStateJsTickets $ Map.filter $ (> expiry) . snd

-- | 为 EndUserQueryResult 增加缓存
wxppAcidSetCachedUserInfo ::
    UTCTime -> WxppAppID -> WxppOpenID -> EndUserQueryResult -> Update WxppAcidState ()
wxppAcidSetCachedUserInfo now app_id open_id qres = do
    modify $ over wxppAcidStateCachedUserInfo $
                Map.insert (open_id, app_id) (TimeTagged now qres)

wxppAcidGetCachedUserInfo ::
    WxppAppID -> WxppOpenID -> Query WxppAcidState (Maybe (TimeTagged EndUserQueryResult))
wxppAcidGetCachedUserInfo app_id open_id =
    asks $ Map.lookup (open_id, app_id) .
            _wxppAcidStateCachedUserInfo

wxppAcidGetCachedSnsUserInfo ::
    WxppAppID
    -> WxppOpenID
    -> Lang
    -> Query WxppAcidState (Maybe (TimeTagged OAuthGetUserInfoResult))
wxppAcidGetCachedSnsUserInfo app_id open_id lang =
    asks $ Map.lookup ((open_id, app_id), lang) . _wxppAcidStateCachedSnsUserInfo

wxppAcidAddCachedSnsUserInfo ::
    WxppAppID
    -> WxppOpenID
    -> Lang
    -> OAuthGetUserInfoResult
    -> UTCTime
    -> Update WxppAcidState ()
wxppAcidAddCachedSnsUserInfo app_id open_id lang info now = do
    modify $ over wxppAcidStateCachedSnsUserInfo $
                Map.union (Map.singleton ((open_id, app_id), lang) (TimeTagged now info))

-- | 查找 UnionID 缓存
wxppAcidLookupCachedUnionID ::
    WxppOpenID -> WxppAppID -> Query WxppAcidState (Maybe (Maybe WxppUnionID, UTCTime))
wxppAcidLookupCachedUnionID open_id app_id =
    asks $ fmap ((endUserQueryResultUnionID . _unTimeTag) &&& _ttTime).
            Map.lookup (open_id, app_id) .
            _wxppAcidStateCachedUserInfo

$(makeAcidic ''WxppAcidState
    [ 'wxppAcidGetAcccessTokens
    , 'wxppAcidGetAcccessToken
    , 'wxppAcidAddAcccessToken
    , 'wxppAcidPurgeAcccessToken
    , 'wxppAcidAddOAuthAcccessToken
    , 'wxppAcidPurgeOAuthAccessToken
    , 'wxppAcidAddJsTicket
    , 'wxppAcidGetJsTicket
    , 'wxppAcidPurgeJsTicket
    , 'wxppAcidLookupOAuthAccessTokens
    , 'wxppAcidLookupUploadedMediaIDByHash
    , 'wxppAcidSaveUploadedMediaID
    , 'wxppAcidSetCachedUserInfo
    , 'wxppAcidGetCachedUserInfo
    , 'wxppAcidLookupCachedUnionID
    , 'wxppAcidGetCachedSnsUserInfo
    , 'wxppAcidAddCachedSnsUserInfo
    ])


newtype WxppCacheByAcid = WxppCacheByAcid (AcidState WxppAcidState)

instance WxppCacheBackend WxppCacheByAcid where
    wxppCacheGetAccessToken (WxppCacheByAcid acid) app_id =
        query acid $ WxppAcidGetAcccessToken app_id

    wxppCacheAddAccessToken (WxppCacheByAcid acid) atk expiry = do
        update acid $ WxppAcidAddAcccessToken atk expiry

    wxppCachePurgeAccessToken (WxppCacheByAcid acid) expiry = do
        update acid $ WxppAcidPurgeAcccessToken expiry

    wxppCacheAddOAuthAccessToken (WxppCacheByAcid acid) atk_p expiry = do
        update acid $ WxppAcidAddOAuthAcccessToken atk_p expiry

    wxppCacheGetOAuthAccessToken (WxppCacheByAcid acid) app_id open_id req_scopes state = do
        infos <- query acid $ WxppAcidLookupOAuthAccessTokens app_id open_id
        return $ fmap fst $ Set.maxView $ Set.filter
                                (\x -> (check_req_scopes $ get_scopes x) && match_state x)
                                infos
        where
            match_state (OAuthTokenInfo _ _ _ s _) = state == s
            get_scopes (OAuthTokenInfo _ _ scopes _ _) = scopes
            check_req_scopes scopes = Set.isSubsetOf req_scopes scopes

    wxppCachePurgeOAuthAccessToken (WxppCacheByAcid acid) expiry = do
        update acid $ WxppAcidPurgeOAuthAccessToken expiry

    wxppCacheGetSnsUserInfo (WxppCacheByAcid acid) app_id open_id lang = runMaybeT $ do
        tt_info <- MaybeT $ query acid $ WxppAcidGetCachedSnsUserInfo app_id open_id lang
        return (_unTimeTag tt_info, _ttTime tt_info)

    wxppCacheAddSnsUserInfo (WxppCacheByAcid acid) app_id open_id lang info now = do
        update acid $ WxppAcidAddCachedSnsUserInfo app_id open_id lang info now

    wxppCacheAddJsTicket (WxppCacheByAcid acid) app_id ticket expiry = do
        update acid $ WxppAcidAddJsTicket app_id ticket expiry

    wxppCacheGetJsTicket (WxppCacheByAcid acid) app_id = do
        now <- liftIO getCurrentTime
        m_tk <- query acid $ WxppAcidGetJsTicket app_id
        case m_tk of
            Nothing -> return Nothing
            Just (_tk, expiry) -> do
                if expiry > now
                    then return m_tk
                    else return Nothing

    wxppCachePurgeJsTicket (WxppCacheByAcid acid) expiry = do
        update acid $ WxppAcidPurgeJsTicket expiry

    wxppCacheLookupUserInfo (WxppCacheByAcid acid) app_id open_id = do
        fmap (fmap $ _unTimeTag &&& _ttTime) $
            query acid $ WxppAcidGetCachedUserInfo app_id open_id

    wxppCacheSaveUserInfo (WxppCacheByAcid acid) app_id qres = do
        now <- liftIO getCurrentTime
        let open_id = endUserQueryResultOpenID qres
        update acid $ WxppAcidSetCachedUserInfo now app_id open_id qres

    wxppCacheLookupUploadedMediaIDByHash (WxppCacheByAcid acid) app_id h = do
        query acid $ WxppAcidLookupUploadedMediaIDByHash app_id h

    wxppCacheSaveUploadedMediaID (WxppCacheByAcid acid) app_id h ures = do
        update acid $ WxppAcidSaveUploadedMediaID app_id h ures
