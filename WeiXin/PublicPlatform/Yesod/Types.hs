module WeiXin.PublicPlatform.Yesod.Types where

import ClassyPrelude

import qualified Network.Wai                as Wai
import Data.Bits                            ((.&.))
import Network.Socket                       (SockAddr(..))


-- | 判断 WAI 请求是否来自可信的来源
-- 有若干 web 接口是打算暴露给同伴使用的
-- 这个函数负责检查这些请求是否可以执行
type RequestAuthChecker = Wai.Request -> IO Bool

alwaysDenyRequestAuthChecker :: RequestAuthChecker
alwaysDenyRequestAuthChecker _ = return False

-- | 总是通过检查
-- 使用这个函数意味着系统有其它手段作安全限制
alwaysAllowRequestAuthChecker :: RequestAuthChecker
alwaysAllowRequestAuthChecker _ = return True

loopbackOnlyRequestAuthChecker :: RequestAuthChecker
loopbackOnlyRequestAuthChecker req = return $ isLoopbackSockAddr $ Wai.remoteHost req

isLoopbackSockAddr :: SockAddr -> Bool
isLoopbackSockAddr addr =
    case addr of
        SockAddrInet _ w        -> w .&. 0xFF  == 127
        SockAddrInet6 _ _ w _   -> w == (0, 0, 0, 1)
        _                       -> False
