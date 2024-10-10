{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.Network.Http where

import Choreography.Location
import qualified Choreography.Network as C
import Data.ByteString (fromStrict)
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API
import Servant.Client (ClientM, client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))
import Servant.Server (Handler, Server, serve)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp (run)

-- * Servant API

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

-- * Http configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String
type Port = Int

-- | Create a HTTP backend configuration from a association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receiving channels

type RecvChans = HashMap LocTm (Chan String)

mkRecvChans :: HttpConfig -> IO RecvChans
mkRecvChans cfg = foldM f HashMap.empty (locs cfg)
  where
    f :: HashMap LocTm (Chan String) -> LocTm
      -> IO (HashMap LocTm (Chan String))
    f hm l = do
      c <- newChan
      return $ HashMap.insert l c hm

-- * HTTP backend

newtype HttpNetwork m a = HttpNetwork { httpNetwork :: HttpConfig -> LocTm -> Manager -> RecvChans -> m a }

instance (Functor f) => Functor (HttpNetwork f) where
  fmap f (HttpNetwork k) = HttpNetwork $ \a b c d -> fmap f (k a b c d)

instance Applicative f => Applicative (HttpNetwork f) where
  pure x = HttpNetwork $ \_ _ _ _ -> pure x
  (HttpNetwork f) <*> (HttpNetwork x) = HttpNetwork $ \a b c d -> f a b c d <*> x a b c d

instance Monad f => Monad (HttpNetwork f) where
  return = pure
  (HttpNetwork ma) >>= f = HttpNetwork $ \a b c d -> ma a b c d >>= ((\v -> httpNetwork v a b c d) . f)


instance MonadIO m => C.NetworkT HttpNetwork m where
  run_ m = HttpNetwork $ \_ _ _ _ -> liftIO (print "running") >> m
  send_ a l = HttpNetwork $ \cfg self mgr chans -> liftIO $ do
    print ("SEND ->" ++ l)
    res <- runClientM (send self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
    case res of
            Left err -> putStrLn $ "Error : " ++ show err
            Right _  -> return ()
    where
      api :: Proxy API
      api = Proxy

      send = client api

  recv_ l = HttpNetwork $ \_ _ mgr chans -> do
          liftIO $ print ("RECV " ++ l)
          liftIO $ read <$> readChan (chans ! l)
  broadcast_ a = HttpNetwork $ \cfg self mgr chans -> httpNetwork (mapM_ (C.send_ a) (locs cfg)) cfg self mgr chans


{-# INLINE runNetworkHttp #-}
runNetworkHttp :: MonadIO m => HttpConfig -> LocTm -> C.Network m a -> m a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftIO $ mkRecvChans cfg
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- runNetworkMain mgr chans prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: (MonadIO m) => Manager -> RecvChans -> C.Network m a -> m a
    runNetworkMain mgr chans (C.Network n) = httpNetwork n cfg self mgr chans

    api :: Proxy API
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
        handler :: LocTm -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (chans ! rmt) msg
          return NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg chans = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server chans)

instance C.Backend HttpConfig where
  runNetwork = runNetworkHttp
