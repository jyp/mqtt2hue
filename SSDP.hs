{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module SSDP (
  getUUIDFromMacAddress, 
        SSDPServiceItem,
        SSDPService(..),
        SSDPHandle,
        startSsdpServer,
        stopSsdpServer,
        getUUID,
        getURL
    ) where

import Network.Socket           (socket, SockAddr, Family(AF_INET), SocketType(Datagram), PortNumber,close) -- sendTo, recvFrom, sClose,
import qualified Network.Socket.ByteString as BS
import Network.Multicast        (setTimeToLive, multicastSender, multicastReceiver, addMembership)
import Control.Exception.Base   (bracket)
import Control.Concurrent       (forkIO, ThreadId, killThread, threadDelay)
import Control.Monad ( forever, forM_, when )
import Data.Time.Clock          (getCurrentTime)
import Data.Time.Format         (formatTime,defaultTimeLocale)
import Network.Info             (NetworkInterface(..),MAC)
import Types (macHex)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Encoding
-- TODO
-- * stop instead of kill threads, and send byebye
-- * catch all exceptions in the threads to make sure they keep running

type SSDPServiceItem = Text

data SSDPService = SSDPService {
        ssInterface      :: NetworkInterface,
        ssPort           :: Int,
        ssURI            :: Text,
        ssOSName         :: Text,
        ssProductName    :: Text,
        ssProductVersion :: Text,
        ssServiceItems   :: [SSDPServiceItem]
    } deriving (Show)


data SSDPHandle = SSDPHandle
  {keepAliveThreadId, listenThreadId :: ThreadId
  ,getUUID, getURL :: Text
  }

serviceUri :: SSDPServiceItem -> Text
serviceUri = id

startSsdpServer :: SSDPService -> IO SSDPHandle
startSsdpServer SSDPService{ssInterface=NetworkInterface{mac,ipv4},..} = do
    let uuid   = getUUIDFromMacAddress mac
        server = ssOSName <> " UPnP/1.0 " <> ssProductName <> "/" <> ssProductVersion  
        base   = Text.pack ("http://" <> show ipv4 <> ":" <> show ssPort)
        url    = base <> ssURI
    a <- forkIO $ sendAlive uuid url server ssServiceItems
    b <- forkIO $ listen    uuid url server ssServiceItems
    return $ SSDPHandle a b base uuid

stopSsdpServer :: SSDPHandle -> IO ()
stopSsdpServer (SSDPHandle {..}) = do
  killThread keepAliveThreadId
  killThread listenThreadId

getUUIDFromMacAddress :: MAC -> Text
getUUIDFromMacAddress mac = 
  Text.pack ("2f402f80-da50-11e1-9b23-" <> macHex mac)

messageTypes :: Text -> [SSDPServiceItem] -> [Text]
messageTypes uuid services = ["upnp:rootdevice", uuid] ++ map serviceUri services

listen :: Text -> Text -> Text -> [SSDPServiceItem] -> IO ()
listen uuid url server services = forever $ do
    (decodeUtf8 -> msg, addr) <- receive
    Text.putStrLn ("{{{ " <> msg)
    when ("M-SEARCH" `Text.isPrefixOf` msg) $
      forM_ (messageTypes uuid services) $ \ msgType -> do
        Text.putStrLn ("??? " <> msgType )
        when (msgType `Text.isInfixOf` msg ||
              "ssdp:all" `Text.isInfixOf` msg) $ do
          Text.putStrLn "!!! Found"
          sendDiscover uuid url server addr msgType
    
getRFC1123Date :: IO Text
getRFC1123Date =
  Text.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
  <$> getCurrentTime

    -- rfc1123-date = wkday "," SP date1 SP time SP "GMT"
    -- wkday        = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
    -- date1        = 2DIGIT SP month SP 4DIGIT         ; day month year (e.g., 02 Jun 1982)
    -- time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT      ; 00:00:00 - 23:59:59
    -- Example: Sun, 06 Nov 1994 08:49:37 GMT


sendDiscover :: Text -> Text -> Text -> SockAddr -> Text -> IO ()
sendDiscover uuid url server addr st = do
    date <- getRFC1123Date
    sendReply addr $ makeMessage "HTTP/1.1 200 OK" [
      ("HOST","239.255.255.250:1900"),
      ("CACHE-CONTROL", "max-age=100"),
      ("DATE", date),
      ("EXT", ""),
      ("LOCATION", url),
      ("SERVER", server),
      ("hue-bridgeid", "9061AEFFFE218F6D"), -- FIXME
      ("ST", (if st == uuid then "uuid:" else "") <> st),
      ("USN", "uuid:" <> uuid <> (if st == uuid then "" else "::" <> st))
      ]

sendAlive :: Text -> Text -> Text -> [SSDPServiceItem] -> IO ()
sendAlive uuid url server services = forever $ do
  putStrLn "Keepalive types:"
  print (messageTypes uuid services)
  send $ map (makeAliveMessage uuid url server) (messageTypes uuid services)
  threadDelay 300000000

makeAliveMessage :: Text -> Text -> Text -> Text -> ByteString
makeAliveMessage uuid url server nt = makeMessage "NOTIFY * HTTP/1.1" [
        ("HOST", "239.255.255.250:1900"),
        ("CACHE-CONTROL", "max-age=100"),
        ("LOCATION", url),
        ("SERVER", server),
        ("NTS", "ssdp:alive"),
        ("hue-bridgeid", "9061AEFFFE218F6D"), -- FIXME
        ("NT", (if nt == uuid then "uuid:" else "") <> nt),
        ("USN", "uuid:" <> uuid <> (if nt == uuid then "" else "::" <> nt))
    ]

-- makeByebyeMessage :: String -> String -> String
-- makeByebyeMessage uuid nt = makeMessage "NOTIFY * HTTP/1.1" [
--         ("HOST", "239.255.255.250:1900"),
--         ("NT", nt),
--         ("NTS", "ssdp:byebye"),
--         ("USN", if nt == uuid then uuid else uuid ++ "::" ++ nt)
--     ]

makeMessage :: Text -> [(Text, Text)] -> ByteString
makeMessage method headers = encodeUtf8 (
    method <> "\r\n" <>
    (Text.intercalate "\r\n" [ key <> ": " <> value | (key, value) <- headers ]) <>
    "\r\n\r\n")

sendReply :: SockAddr -> ByteString -> IO ()
sendReply addr message = do
    bracket
        (socket AF_INET Datagram 0)
        close
        (\sock -> forM_ ([1..3] :: [Int]) $ \_ -> do
            BS.sendAllTo sock message addr
            threadDelay 500000)

ssdpIP :: String
ssdpIP   = "239.255.255.250"

ssdpPort :: PortNumber
ssdpPort = 1900

send :: [ByteString] -> IO ()
send messages = do
    bracket
        (multicastSender ssdpIP ssdpPort)
        (close . fst)
        (\(sock, addr) -> do
            setTimeToLive sock 4
            addMembership sock ssdpIP Nothing
            forM_ ([1..3] :: [Int]) $ \_ -> do
                forM_ messages $ \m -> do
                    BS.sendAllTo sock m addr
                threadDelay 500000)

receive :: IO (ByteString, SockAddr)
receive = do
    bracket
        (multicastReceiver ssdpIP ssdpPort)
        close
        (\sock -> do 
            (msg, addr) <- BS.recvFrom sock 1024
            return (msg, addr))

-----------------------------------------

                    
exampleTypes :: [Text]
exampleTypes = ["upnp:rootdevice","2f402f80-da50-11e1-9b23-9061ae218f6d","urn:schemas-upnp-org:device:basic:1"]

testAlives :: [Text]
testAlives = map (decodeUtf8 . makeAliveMessage "2f402f80-da50-11e1-9b23-9061ae218f6d" "url" "descr" ) exampleTypes

-- >>> mapM_ Text.putStrLn testAlives
-- NOTIFY * HTTP/1.1
-- HOST: 239.255.255.250:1900
-- CACHE-CONTROL: max-age=100
-- LOCATION: url
-- SERVER: descr
-- NTS: ssdp:alive
-- hue-bridgeid: 9061AEFFFE218F6D
-- NT: upnp:rootdevice
-- USN: uuid:2f402f80-da50-11e1-9b23-9061ae218f6d::upnp:rootdevice
-- NOTIFY * HTTP/1.1
-- HOST: 239.255.255.250:1900
-- CACHE-CONTROL: max-age=100
-- LOCATION: url
-- SERVER: descr
-- NTS: ssdp:alive
-- hue-bridgeid: 9061AEFFFE218F6D
-- NT: uuid:2f402f80-da50-11e1-9b23-9061ae218f6d
-- USN: uuid:2f402f80-da50-11e1-9b23-9061ae218f6d
-- NOTIFY * HTTP/1.1
-- HOST: 239.255.255.250:1900
-- CACHE-CONTROL: max-age=100
-- LOCATION: url
-- SERVER: descr
-- NTS: ssdp:alive
-- hue-bridgeid: 9061AEFFFE218F6D
-- NT: urn:schemas-upnp-org:device:basic:1
-- USN: uuid:2f402f80-da50-11e1-9b23-9061ae218f6d::urn:schemas-upnp-org:device:basic:1
