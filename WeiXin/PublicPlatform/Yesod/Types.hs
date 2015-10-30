module WeiXin.PublicPlatform.Yesod.Types where

import ClassyPrelude.Yesod hiding (Request, requestHeaders)
import qualified Data.ByteString            as B

import Network.Wai                          (Request)
import Data.Bits                            ((.&.))
import Data.Char                            (isDigit, chr)
import Network.Socket                       (SockAddr(..))
import Network.Wai                          (remoteHost, requestHeaders)


-- | 判断 WAI 请求是否来自可信的来源
-- 有若干 web 接口是打算暴露给同伴使用的
-- 这个函数负责检查这些请求是否可以执行
type RequestAuthChecker = Request -> IO Bool

alwaysDenyRequestAuthChecker :: RequestAuthChecker
alwaysDenyRequestAuthChecker _ = return False

-- | 总是通过检查
-- 使用这个函数意味着系统有其它手段作安全限制
alwaysAllowRequestAuthChecker :: RequestAuthChecker
alwaysAllowRequestAuthChecker _ = return True

loopbackOnlyRequestAuthChecker :: RequestAuthChecker
loopbackOnlyRequestAuthChecker req = return $ isLoopbackSockAddr $ remoteHost req

isLoopbackSockAddr :: SockAddr -> Bool
isLoopbackSockAddr addr =
    case addr of
        SockAddrInet _ w        -> w .&. 0xFF  == 127
        SockAddrInet6 _ _ w _   -> w == (0, 0, 0, 1)
        _                       -> False


-- | 从 User-Agent 找微信版本
handlerGetWeixinClientVersion :: MonadHandler m => m (Maybe ByteString)
handlerGetWeixinClientVersion = do
    req <- waiRequest
    let headers = requestHeaders req
    return $ join $ fmap parse_header $ lookup hUserAgent headers
    where
        parse_header h = do
            let prefix = "MicroMessenger/"
                mm_start = snd $ B.breakSubstring prefix h
            guard $ B.isPrefixOf prefix mm_start
            return $ B.takeWhile ((\c -> isDigit c || c == '.') . chr . fromIntegral) $
                        B.drop (length prefix) mm_start
