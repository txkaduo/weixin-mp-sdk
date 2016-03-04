module WeiXin.PublicPlatform.CloudHaskell where

import           ClassyPrelude                      hiding (newChan)
import qualified Data.ByteString.Lazy               as LB
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Data.Aeson
import           Data.Binary                        (Binary (..))
import           System.Timeout                     (timeout)

import           WeiXin.PublicPlatform.InMsgHandler
import           WeiXin.PublicPlatform.Types


data SimpleCloudBackend = SimpleCloudBackend
                            (IO LocalNode)
                              -- ^ create new LocalNode
                            (IO [NodeId])
                              -- ^ send to these Nodes

-- | A middleware to send event notifications of some types to the cloud (async)
data TeeEventToCloud = TeeEventToCloud
                          WxppAppID
                          SimpleCloudBackend
                          String
                            -- ^ Process name that should receive forwarded message
                          [Text]
                            -- ^ event names to forward (wxppEventTypeString)
                            -- if null, forward all.

instance JsonConfigable TeeEventToCloud where
    type JsonConfigableUnconfigData TeeEventToCloud =
            (WxppAppID, SimpleCloudBackend)

    -- | 假定每个算法的配置段都有一个 name 的字段
    -- 根据这个方法选择出一个指定算法类型，
    -- 然后从 json 数据中反解出相应的值
    isNameOfInMsgHandler _ = (== "tee-to-cloud")

    parseWithExtraData _ (x1, x2) o = TeeEventToCloud x1 x2
                                        <$> o .: "receive-proc-name"
                                        <*> o .: "event-types"

instance MonadIO m => IsWxppInMsgProcMiddleware m TeeEventToCloud where
    preProcInMsg
      (TeeEventToCloud app_id (SimpleCloudBackend new_local_node find_peers) pname evt_types)
      _cache bs m_ime = do
          forM_ m_ime $ \ime -> do
            case wxppInMessage ime of
              WxppInMsgEvent evt -> do
                when (null evt_types || wxppEventTypeString evt `elem` evt_types) $ do
                  node <- liftIO new_local_node
                  peers <- liftIO find_peers
                  liftIO $ runProcess node $ do
                    my_pid <- getSelfPid
                    forM_ peers $ \nid -> do
                      let msg = WrapInMsgHandlerInput app_id bs ime
                      nsendRemote nid pname (my_pid, msg)

              _ -> return ()

          return $ Just (bs, m_ime)


-- | A message handler that send WxppInMsgEntity to peers and wait for responses
data DelegateInMsgToCloud (m :: * -> *) =
                          DelegateInMsgToCloud
                              WxppAppID
                              SimpleCloudBackend
                              String
                                -- ^ Process name that should receive forwarded message
                              Int
                                -- ^ timeout (ms) when selecting processes to handle
                                -- 配置时用的单位是秒，浮点数
                              Int
                                -- ^ timeout (ms) when handling message with Cloud Haskell
                                -- 配置时用的单位是秒，浮点数

instance JsonConfigable (DelegateInMsgToCloud m) where
    type JsonConfigableUnconfigData (DelegateInMsgToCloud m) =
            (WxppAppID, SimpleCloudBackend)

    -- | 假定每个算法的配置段都有一个 name 的字段
    -- 根据这个方法选择出一个指定算法类型，
    -- 然后从 json 数据中反解出相应的值
    isNameOfInMsgHandler _ = (== "deletgate-to-cloud")

    parseWithExtraData _ (x1, x2) o = DelegateInMsgToCloud x1 x2
                                  <$> o .: "receive-proc-name"
                                  <*> (fmap (round . (* 1000000)) $
                                          o .:? "timeout1" .!= (0.1 :: Float)
                                      )
                                      -- ^ timeout number is a float in seconds
                                  <*> (fmap (round . (* 1000000)) $
                                          o .:? "timeout2" .!= (5 :: Float)
                                      )
                                      -- ^ timeout number is a float in seconds

type instance WxppInMsgProcessResult (DelegateInMsgToCloud m) = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => IsWxppInMsgProcessor m (DelegateInMsgToCloud m) where
    processInMsg
      (DelegateInMsgToCloud app_id (SimpleCloudBackend new_local_node find_peers) pname t1 t2)
      _cache bs m_ime = do
        case m_ime of
          Nothing   -> return $ Right []
          Just ime  -> do
            node <- liftIO new_local_node
            peers <- liftIO find_peers
            let select_pid = do
                  my_pid <- getSelfPid
                  forM_ peers $ \nid -> nsendRemote nid pname $ WxppElectInMsgHandler my_pid
                  WxppElectInMsgHandlerR pid <- expect
                  return pid

            let send_recv pid = do
                  (send_port, recv_port) <- newChan
                  let msg = WrapInMsgHandlerInput app_id bs ime
                  send pid (msg, send_port)
                  receiveChan recv_port

            let get_answer = runMaybeT $ do
                  pid <- MaybeT (runProcessTimeout t1 node select_pid)
                  MaybeT $ runProcessTimeout t2 node $ send_recv pid

            let handle_err err = do
                  $logErrorS wxppLogSource $
                    "got exception when running cloud-haskell code: "
                    <> tshow err
                  return $ Left $ show err

            let handle_timeout = do
                  $logErrorS wxppLogSource $
                    "Timed-out when running cloud-haskell code: "
                    <> "timeout1=" <> tshow (fromIntegral t1 / 1000 / 1000 :: Float)
                    <> ", timeout2=" <> tshow (fromIntegral t2 / 1000 / 1000 :: Float)
                  return $ Left "timed-out in cloud"

            (liftIO get_answer >>= maybe handle_timeout return)
              `catchAny` handle_err


-- | A message to tell all candidate processes to response
-- if they want to handle a WxppInMsgEntity
data WxppElectInMsgHandler = WxppElectInMsgHandler ProcessId
                            deriving (Typeable, Generic)
instance Binary WxppElectInMsgHandler

-- | Response to WxppElectInMsgHandler
data WxppElectInMsgHandlerR = WxppElectInMsgHandlerR ProcessId
                            deriving (Typeable, Generic)
instance Binary WxppElectInMsgHandlerR

-- | Cloud message that wraps incoming message info
data WrapInMsgHandlerInput = WrapInMsgHandlerInput
                                WxppAppID
                                LB.ByteString
                                WxppInMsgEntity
                            deriving (Typeable, Generic)
instance Binary WrapInMsgHandlerInput

runProcessTimeout :: Int -> LocalNode -> Process a -> IO (Maybe a)
runProcessTimeout t node proc = do
  mv <- newEmptyMVar
  timeout t $ do
    runProcess node $ do
      r <- proc
      liftIO $ putMVar mv r
    readMVar mv
