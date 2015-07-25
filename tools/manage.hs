module Main where

import ClassyPrelude hiding ((<>), FilePath, (</>), (<.>))
import Control.Monad.Logger
import Options.Applicative
import Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Yaml                  as Y
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Map.Lazy              as LM
import Filesystem.Path.CurrentOS            (FilePath, fromText)
import Network.Mime                         (defaultMimeMap, MimeType)
import System.IO                            (hFlush, openTempFile, readLn, hSeek, SeekMode(..))
import System.Directory                     (getTemporaryDirectory, removeFile)
import System.Process                       (callProcess)
import System.Environment                   (lookupEnv)
import Data.List                            ((!!))
import Data.Conduit
import Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.Conduit.List          as CL
-- import Control.Monad.Reader                 (asks)

import System.Log.FastLogger                (pushLogStr, newStderrLoggerSet, LoggerSet)

import WeiXin.PublicPlatform.AutoReplyRules
import WeiXin.PublicPlatform.Menu
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.Material
-- import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Security

data ManageCmd = QueryAutoReplyRules
                | QueryOriginMenu
                | QueryCurrentMenu
                | LoadMenu FilePath
                | GetDurable
                    WxppDurableMediaID
                    Bool    -- ^ edit if true, save otherwise. Only valid for news.
                | CountDurable
                | ListAllDurableMedia WxppMediaType
                | GetAllDurableNews
                    Bool    -- ^ show durable media id only
                | SearchDurableNewsByTitle
                    Bool    -- ^ edit if true, show otherwise
                    Text
                | ListGroup
                | CreateGroup Text
                | DeleteGroup WxppUserGroupID
                | RenameGroup WxppUserGroupID Text
                | GroupOfUser WxppOpenID
                | SetUserGroup WxppUserGroupID [WxppOpenID]
                deriving (Show, Eq, Ord)


data Options = Options {
                optAppID        :: WxppAppID
                , optAppToken   :: Maybe Token
                , optAppSecret  :: Maybe WxppAppSecret
                , optAppAesKey  :: Maybe AesKey
                , optAppAccessTokenData :: Maybe Text
                , optEditor     :: Maybe Text
                , optVerbose    :: Int
                , optCommand    :: ManageCmd
                }


manageCmdParser :: Parser ManageCmd
manageCmdParser = subparser $
    command "query-autoreply-rules"
        (info (helper <*> pure QueryAutoReplyRules)
            (progDesc "取当前自动回复规则设置"))
    <> command "query-origin-menu"
        (info (helper <*> pure QueryOriginMenu)
            (progDesc "取自定义菜单配置（即在官方后台管理界面中的设置）"))
    <> command "load-menu"
        (info (helper <*> (fmap LoadMenu $ fmap fromString $ argument str (metavar "YAML_FILE")))
            (progDesc "加载菜单设置"))
    <> command "query-current-menu"
        (info (helper <*> pure QueryCurrentMenu)
            (progDesc "取当前通过程序设定的菜单"))
    <> command "get-durable-media"
        (info (helper <*> (flip GetDurable
                            <$> switch (long "edit" <> help "edit it if it is news")
                            <*> ((WxppDurableMediaID . fromString) <$> argument str (metavar "MEDIA_ID"))
                        ))
            (progDesc "下载（获取）永久素材"))
    <> command "list-all-durable-media"
        (info (helper <*> (ListAllDurableMedia <$> argument mediaTypeReader (metavar "MEDIA_TYPE")))
            (progDesc "列出特定类型的所有多媒体永久素材"))
    <> command "get-all-durable-news"
        (info (helper <*> (GetAllDurableNews
                            <$> switch (long "show-id-only" <> help "show durable media id only")))
            (progDesc "取永久素材中的所有图文消息"))
    <> command "search-durable-news-title"
        (info (helper <*> (SearchDurableNewsByTitle
                            <$> switch (long "edit" <> help "edit the located news")
                            <*> (fromString <$> argument str (metavar "STRING"))
                            ))
            (progDesc "根据搜索永久素材中标题含有指定关键字的图文消息"))
    <> command "count-durable-media"
        (info (helper <*> pure CountDurable)
            (progDesc "统计永久素材数量"))
    <> command "list-groups"
        (info (helper <*> pure ListGroup)
            (progDesc "列出所有用户分组"))
    <> command "new-group"
        (info (helper <*> (CreateGroup <$> (fmap fromString $ argument str (metavar "NAME"))))
            (progDesc "新建用户分组"))
    <> command "del-group"
        (info (helper <*> (DeleteGroup <$> (fmap WxppUserGroupID $ argument auto (metavar "GROUP_ID"))))
            (progDesc "删除用户分组"))
    <> command "rename-group"
        (info (helper <*> (RenameGroup
                            <$> (fmap WxppUserGroupID $ argument auto (metavar "GROUP_ID"))
                            <*> (fmap fromString $ argument str (metavar "NEW_NAME"))
                        )
                )
            (progDesc "修改用户分组名"))
    <> command "group-of-user"
        (info (helper <*> (GroupOfUser
                            <$> (fmap (WxppOpenID . fromString) $ argument str (metavar "USER_OPEN_ID"))
                        )
                )
            (progDesc "取用户所在分组"))
    <> command "set-user-group"
        (info (helper <*> (SetUserGroup
                            <$> (fmap WxppUserGroupID $ argument auto (metavar "GROUP_ID"))
                            <*> (some $ fmap (WxppOpenID . fromString) $ argument str (metavar "USER_OPEN_ID"))
                            )
                )
            (progDesc "移动用户至指定分组"))


mediaTypeReader ::
#if MIN_VERSION_optparse_applicative(0, 11, 0)
    ReadM WxppMediaType
mediaTypeReader = do
    s <- str
#else
    (Monad m) => String -> m WxppMediaType
mediaTypeReader s = do
#endif
    maybe (fail $ "invalid media type: " <> s) return $
        lookup s s_to_type_list
    where
        s_to_type_list = map (wxppMediaTypeString &&& id) [minBound..maxBound]


aesKeyReader ::
#if MIN_VERSION_optparse_applicative(0, 11, 0)
    ReadM AesKey
aesKeyReader = do
    s <- str
#else
    (Monad m) => String -> m AesKey
aesKeyReader s = do
#endif
    either fail return $ decodeBase64AesKey $ fromString s

optionsParse :: Parser Options
optionsParse = Options
                <$> (WxppAppID . fromString <$> strOption (long "app-id"
                                                <> metavar "APP_ID"
                                                <> help "App ID String"))
                <*> (optional $ Token . fromString <$> strOption (long "token"
                                                <> metavar "TOKEN"
                                                <> help "App Token String"))
                <*> (optional $ WxppAppSecret . fromString <$> strOption
                                    (long "secret"
                                    <> metavar "SECRET"
                                    <> help "App Secret String"))
                <*> (optional $ option aesKeyReader
                                    (long "aes-key"
                                    <> metavar "AES_KEY"
                                    <> help "Base64-encoded App AES Key"))
                <*> (optional $ fromString <$> strOption
                                    (long "access-token"
                                    <> metavar "ACCESS_TOKEN"
                                    <> help "Provide an known Access Token String of the App rather than get a new one."))
                <*> (optional $ fromString <$> strOption
                                    (long "editor"
                                    <> metavar "EDITOR_CMD"
                                    <> help "external editor command"))
                <*> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> manageCmdParser


start :: (MonadLogger m, MonadThrow m, MonadCatch m, MonadIO m) => ReaderT Options m ()
start = do
    opts <- ask
    let app_id = optAppID opts
        m_app_secret = optAppSecret opts
        get_atk = do
            case optAppAccessTokenData opts of
                Nothing -> do
                    case m_app_secret of
                        Nothing -> do
                            throwM $ userError "need either app secret or access token"
                        Just app_secret -> do
                            AccessTokenResp atk_p _ttl <- refreshAccessToken' app_id app_secret
                            return $ atk_p app_id

                Just atk_data -> do
                    return $ AccessToken atk_data app_id

    case optCommand opts of

        QueryAutoReplyRules -> do
            atk <- get_atk
            obj <- wxppQueryOriginAutoReplyRules atk
            liftIO $ B.putStr $ Y.encode obj

        QueryOriginMenu -> do
            atk <- get_atk
            result <- wxppQueryMenuConfig atk
            liftIO $ B.putStr $ Y.encode result

        LoadMenu fp -> do
            atk <- get_atk
            result <- wxppCreateMenuWithYaml atk (fromText "." :| []) fp
            case result of
                Left err -> do
                    $logError $ fromString $
                        "wxppCreateMenuWithYaml failed: " ++ show err
                Right _ -> return ()

        QueryCurrentMenu -> do
            result <- get_atk >>= wxppQueryMenu
            liftIO $ B.putStr $ Y.encode result

        GetDurable mid edit_mode -> do
            atk <- get_atk
            result <- wxppGetDurableMedia atk mid

            case result of
                WxppGetDurableNews articles -> do
                    if edit_mode
                        then editNewsDurable atk mid articles
                        else liftIO $ B.putStr $ Y.encode articles

                WxppGetDurableVideo title desc down_url -> do
                    liftIO $ B.putStr $ Y.encode $ object
                        [ "title"           .= title
                        , "description"     .= desc
                        , "introduction"    .= desc
                        , "download_url"    .= unUrlText down_url
                        ]

                WxppGetDurableRaw mime bs -> do
                    let ext = fromMaybe "dat" $ extByMime mime
                        fn  = unWxppDurableMediaID mid <> "." <> ext
                    liftIO $ do
                        putStr "Content-Type: "
                        B.putStr mime
                        putStrLn ""
                        LB.writeFile (T.unpack fn) bs
                        putStrLn $ "Saved to file: " <> fn

        CountDurable -> do
            atk <- get_atk
            result <- wxppCountDurableMedia atk
            liftIO $ B.putStr $ Y.encode result

        ListGroup -> do
            atk <- get_atk
            result <- wxppListUserGroups atk
            liftIO $ B.putStr $ Y.encode result

        CreateGroup name -> do
            atk <- get_atk
            grp_id <- wxppCreateUserGroup atk name
            liftIO $ putStrLn $ "group created, id is " <> (fromString $ show $ unWxppUserGroupID grp_id)

        DeleteGroup grp_id -> do
            atk <- get_atk
            wxppDeleteUserGroup atk grp_id

        RenameGroup grp_id name -> do
            atk <- get_atk
            wxppRenameUserGroup atk grp_id name

        GroupOfUser open_id -> do
            atk <- get_atk
            grp_id <- wxppGetGroupOfUser atk open_id
            liftIO $ putStrLn $ "user's group id is " <> (fromString $ show $ unWxppUserGroupID grp_id)

        SetUserGroup grp_id open_id_list -> do
            atk <- get_atk
            case open_id_list of
                []          -> return ()
                [open_id]   -> wxppSetUserGroup atk grp_id open_id
                _           -> wxppBatchSetUserGroup atk grp_id open_id_list

        ListAllDurableMedia mtype -> do
            atk <- get_atk
            wxppBatchGetDurableToSrc (wxppBatchGetDurableMedia atk mtype 20)
                $=  CL.map toJSON
                $$ CL.mapM_ (liftIO . B.putStr . Y.encode)

        GetAllDurableNews show_id_only -> do
            atk <- get_atk
            wxppBatchGetDurableToSrc (wxppBatchGetDurableNews atk 20)
                $=  CL.map (if show_id_only
                                then toJSON . unWxppDurableMediaID . wxppBatchGetDurableNewsItemID
                                else toJSON
                            )
                $$ CL.mapM_ (liftIO . B.putStr . Y.encode)

        SearchDurableNewsByTitle edit_mode keyword -> do
            atk <- get_atk
            let has_keyword item =
                    let articles = wxppBatchGetDurableNewsItemContent item
                    in any (T.isInfixOf keyword . wxppDurableArticleTitle) articles

            results <- wxppBatchGetDurableToSrc (wxppBatchGetDurableNews atk 20)
                $= (awaitForever $ \x -> do
                        liftIO $ putStr "." >> hFlush stdout
                        yield x
                )
                $= CL.filter has_keyword
                $$ CL.consume

            if edit_mode
                then do
                    m_the_one <- liftIO $ chooseOne
                                    (unWxppDurableMediaID . wxppBatchGetDurableNewsItemID)
                                    results
                    case m_the_one of
                        Nothing -> return ()
                        Just result -> do
                            let mid = wxppBatchGetDurableNewsItemID result
                            editNewsDurable atk mid (wxppBatchGetDurableNewsItemContent result)
                else do
                    mapM_ (liftIO . B.putStr . Y.encode) results


editNewsDurable :: (MonadIO m, MonadLogger m, MonadCatch m) =>
    AccessToken -> WxppDurableMediaID -> [WxppDurableArticle] -> m ()
editNewsDurable atk mid articles = do
    let go bs = do
            bs' <- liftIO $ editWithEditor bs
            case Y.decodeEither bs' of
                Left err -> do
                    answer <- liftIO $ do
                        putStrLn $ fromString err
                        putStr "try again?"
                        hFlush stdout
                        T.getLine
                    if T.toLower answer `elem` [ "y", "yes" ]
                        then go bs'
                        else return ()

                Right new_articles -> do
                    let old_len = length articles
                    forM_ (zip [0..old_len] $ zip articles new_articles) $ \(idx, (old_a, article)) -> do
                        when (old_a /= article) $ do
                            wxppReplaceArticleOfDurableNews atk mid idx article

    putStrLn $ "launch editor to edit durable news: " <> unWxppDurableMediaID mid
    go (Y.encode articles)


chooseOne :: (a -> Text) -> [a] -> IO (Maybe a)
chooseOne _prompt []    = return Nothing
chooseOne _prompt [x]   = return $ Just x
chooseOne prompt  xs    = do
    forM_ (zip [1::Int ..] xs) $ \(idx, x) -> do
        putStrLn $ fromString (show idx) <> ") " <> prompt x
    putStrLn $ "choose one (0 to exit): "
    hFlush stdout
    choice <- readLn
    case choice of
        0 -> return Nothing
        c -> if c < 0 || c > length xs
                then do
                    putStrLn "invalid choice"
                    chooseOne prompt xs
                else
                    return $ Just $ xs !! (c - 1)

editWithEditor :: ByteString -> IO ByteString
editWithEditor bs = do
    tmp_dir <- getTemporaryDirectory
    -- create a tmp file
    bracket
        (openTempFile tmp_dir "wxpp-manage-")
        (\(tmp_fp, tmp_h) -> hClose tmp_h >> removeFile tmp_fp)
        $ \(tmp_fp, tmp_h) -> do
            -- launch editor
            B.hPut tmp_h bs
            hFlush tmp_h
            cmd <- fmap (fromMaybe "vi") $ lookupEnv "EDITOR"
            callProcess cmd [ tmp_fp ]
            hSeek tmp_h AbsoluteSeek 0
            B.hGetContents tmp_h >>= (return $!)

-- | 根据 mime 反查出一个扩展名
extByMime :: MimeType -> Maybe Text
extByMime mime =
    listToMaybe $ LM.keys $ LM.filter (== mime) defaultMimeMap

start' :: Options -> IO ()
start' opts = do
    logger_set <- newStderrLoggerSet 0
    runLoggingT
        (runReaderT start opts)
        (appLogger logger_set (optVerbose opts))

appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
appLogger logger_set verbose loc src level ls = do
    let should_log = case level of
                        LevelOther {}   -> True
                        _               -> level `elem` lv_by_v verbose

    if should_log
        then pushLogStr logger_set $ defaultLogStr loc src level ls
        else return ()
    where
        lv_by_v lv
            | lv <= 0   = [ LevelError]
            | lv == 1   = [ LevelError, LevelWarn ]
            | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
            | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]

main :: IO ()
main = execParser opts >>= start'
    where
        opts = info (helper <*> optionsParse)
                ( fullDesc
                    <> progDesc (unlines
                        [ "执行一些微信公众平台管理查询操作"
                        , "警告：执行操作会更新 access token，服务器使用中的access token可能会失效"
                        , "建议从命令行参数直接指定 access token"
                        ])
                    <> header "wxpp-manage - 微信公众平台管理查询小工具"
                    )
