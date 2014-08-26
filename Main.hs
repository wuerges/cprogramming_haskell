import Protocol
import Request
import Peer
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Control.Monad (unless)
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU

server_port = 3006
server_ip = "127.0.0.1" 

client = withSocketsDo $ bracket getSocket sClose talk
    where getSocket = do
            (serveraddr:_) <- getAddrInfo Nothing (Just server_ip) (Just $ show server_port)
            s <- socket (addrFamily serveraddr) Datagram defaultProtocol
            connect s (addrAddress serveraddr) >> return s
          talk s = do
            send s $ Request.encode $ GetPeersRequest (Peer (T.pack server_ip) server_port (T.pack "aoeuaoeu") )
            recv s 1024 >>= \msg -> putStrLn $ "Received " ++ (SU.unpack msg)
            talk s

server = withSocketsDo $ bracket connectMe sClose handler
    where connectMe = do
            (serveraddr:_) <- getAddrInfo
                                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just $ show  server_port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bindSocket sock (addrAddress serveraddr) >> return sock

handler :: Socket -> IO ()
handler conn = do
    (msg,d) <- recvFrom conn 1024
    putStrLn $ "< " ++ (SU.unpack msg)
    unless (SU.null msg) $ sendTo conn msg d >> handler conn

main = do
    forkIO server
    threadDelay 1000
    client
