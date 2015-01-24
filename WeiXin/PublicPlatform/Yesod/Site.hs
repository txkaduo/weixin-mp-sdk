{-# OPTIONS_GHC -fno-warn-orphans #-}
module WeiXin.PublicPlatform.Yesod.Site
    ( module WeiXin.PublicPlatform.Yesod.Site
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    ) where

import ClassyPrelude
import Yesod
import qualified Data.ByteString.Base16     as B16
import qualified Data.Text                  as T
import Yesod.Helpers.Handler                ( httpErrorWhenParamError
                                            , reqGetParamE'
                                            , paramErrorFromEither
                                            )


import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Security

getSubHomeR :: Yesod master => HandlerT WxppSub (HandlerT master IO) Text
getSubHomeR = do
    foundation <- getYesod

    let token = wxppSubToken foundation

        check_sign (tt, nn, sign) =
            if B16.encode sign0 == encodeUtf8 ( T.toLower sign )
                then Right ()
                else Left $ "invalid signature"
            where
                sign0 = wxppSignature token tt nn ""

    (httpErrorWhenParamError =<<) $ do
        -- check required params
        sign <- reqGetParamE' "signature"
        tt <- liftM (fmap TimeStampS) $ reqGetParamE' "timestamp"
        nn <- liftM (fmap Nonce) $ reqGetParamE' "nonce"
        es <- reqGetParamE' "echostr"
        let dat = (,,) <$> tt <*> nn <*> sign
            res = dat >>= paramErrorFromEither "signature" . check_sign
        return $ res *> es


instance Yesod master => YesodSubDispatch WxppSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWxppSub)
