module WeiXin.PublicPlatform.Yesod.Utils where

-- {{{1 imports
import           ClassyPrelude.Yesod hiding (requestHeaders)
import qualified Data.ByteString     as B
import           Data.Char           (chr, isDigit)
import           Network.Wai         (requestHeaders)

import           Yesod.Helpers.JSend
-- }}}1

-- | 小工具: 显示一个页面，提示用户须在微信内打开当前页面
promptReqOpenInWx :: MonadWidget m => m ()
-- {{{1
promptReqOpenInWx = do
  addScriptRemote "//cdn.bootcss.com/jquery.qrcode/1.0/jquery.qrcode.min.js"
  toWidget [hamlet|
    <div #prompt-open-in-wx>
      <p> 这个页面需要在微信中打开
      <ul> 您可以使用以下方法之一
        <li>请微信扫描以下二维码
          <div #qrcode_area>
        <li>使用浏览器自身的分享功能，把这个页面发送给自己
        <li>
          <button .copy_link>复制链接地址
  |]

  toWidget [julius|
    function copyTextToClipboard(text) {
      var textArea = document.createElement("textarea");
      textArea.style.display = "none";
      textArea.value = text;
      document.body.appendChild(textArea);
      textArea.select();
      var msg;
      try {
        var success = document.execCommand("copy");
        msg = success ? "成功复制到剪贴板" : "复制失败";
      } catch (err){
        console.log("fail to copy");
        msg =  "复制失败";
      }
      document.body.removeChild(textArea);
      alert(msg);
    }

    function init() {
      var qrcode_url = windows.location.href;
      var qrcode = new QRCode(document.getElementById("qrcode_area"), qrcode_url);
      var prompt_wx = document.getElementById("prompt-open-in-wx");
      var copy_button = prompt_wx.getElementsByTagName("button")[0];
      copy_button.addEventListener("click", function(){
          copyTextToClipboard(qrcode_url);
          });
    }

    $() {
      init();
    }
  |]

  toWidget [lucius|
    #prompt-open-in-wx {
      text-align: center;
      padding-top: 30px;
      background: #f4f4f4;
      position: fixed;
      width: 100%;
      height: 100%
    }

    #qrcode_area{
      margin: 0 8%;
    }

    #qrcode_area img {
      margin: 0 auto;
    }
  |]
-- }}}1


promptReqOpenInWxTC :: Yesod site => HandlerT site IO TypedContent
-- {{{1
promptReqOpenInWxTC = do
  selectRep $ do
    provideRep $ do
      defaultLayout promptReqOpenInWx

    provideRepJsendAndJsonp $ do
      return $ JSendFail $
         object [ "msg" .= asText "请在微信内打开此页面"
                , "require_in_wx" .= True
                , "reason" .= asText "require_in_wx"
                ]
-- }}}1


-- | shortcircuited if not opened in WX
forceReqOpenInWxTC :: Yesod site => HandlerT site IO ()
-- {{{1
forceReqOpenInWxTC = do
  is_client_wx <- isJust <$> handlerGetWeixinClientVersion
  unless is_client_wx $ do
    promptReqOpenInWxTC >>= sendResponse
-- }}}1



-- | 从 User-Agent 找微信版本
handlerGetWeixinClientVersion :: MonadHandler m => m (Maybe ByteString)
-- {{{1
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
-- }}}1


-- vim: set foldmethod=marker:
