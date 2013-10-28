module IRC
  (MonadConnection(..),
   ConnectionState,
   dropOutgoingMessageQueue,
   ConnectionT,
   runConnectionT,
   Message(..),
   Event(..),
   messageSenderNickname,
   parseMessage,
   formatMessage,
   parseRecipient,
   sourceIRC,
   send)
  where

import Import

import qualified Data.ByteString as ByteString
import qualified Data.Conduit.Binary as ConduitBinary
import qualified Data.Conduit.List as ConduitList
import qualified Data.Conduit.Text as ConduitText
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Network.Socket as Network
import qualified System.Random as Random
import qualified System.IO as IO


data Message =
  Message {
      messageSender :: Maybe Text.Text,
      messageCommand :: Text.Text,
      messageParameters :: [Text.Text]
    }


data Event
  = MessageEvent Message
  | ConnectEvent
  | DisconnectEvent


messageSenderNickname :: Message -> Maybe Text.Text
messageSenderNickname message =
  fmap ((\(nickname, _, _) -> nickname) . parseRecipient)
       (messageSender message)


parseMessage :: Text.Text -> Maybe Message
parseMessage line = do
  let isSpace :: Char -> Bool
      isSpace ' ' = True
      isSpace _ = False
      breakAtSpace :: Text.Text -> (Text.Text, Text.Text)
      breakAtSpace text =
        let (before, duringAndAfter) = Text.break isSpace text
            after = Text.dropWhile isSpace duringAndAfter
        in (before, after)
      startsWithColon :: Text.Text -> Bool
      startsWithColon text = (Text.length text >= 1) && (Text.head text == ':')
  (maybeSender, line) <- if startsWithColon line
                           then let (sender, rest) =
                                      breakAtSpace $ Text.drop 1 line
                                in return (Just sender, rest)
                           else return (Nothing, line)
  (command, line) <- return $ breakAtSpace line
  let takeParameters :: Text.Text -> [Text.Text]
      takeParameters "" = []
      takeParameters text | startsWithColon text = [Text.drop 1 text]
                          | otherwise =
                              let (parameter, rest) = breakAtSpace text
                              in parameter : takeParameters rest
  parameters <- return $ takeParameters line
  return $ Message {
               messageSender = maybeSender,
               messageCommand = command,
               messageParameters = parameters
             }


formatMessage :: Message -> Text.Text
formatMessage message =
  Text.intercalate " "
    $ (case messageSender message of
         Nothing -> []
         Just sender -> [Text.append ":" sender])
      ++ [messageCommand message]
      ++ (case messageParameters message of
            [] -> []
            parameters -> (List.init parameters)
                          ++ [Text.append ":" (List.last parameters)])


parseRecipient :: Text.Text -> (Text.Text, Text.Text, Text.Text)
parseRecipient recipient =
  let (nickname, afterNickname) =
        Text.break (\c -> c == '!') recipient
      (ident, afterIdent) =
        Text.break (\c -> c == '@') (Text.drop 1 afterNickname)
      hostmask = Text.drop 1 afterIdent
  in (nickname, ident, hostmask)


class (Monad m) => MonadConnection m where
  getConnectionState :: m ConnectionState
  setConnectionState :: ConnectionState -> m ()
instance (MonadConnection m) => MonadConnection (ConduitM i o m) where
  getConnectionState = lift getConnectionState
  setConnectionState state = lift $ setConnectionState state


data ConnectionState =
  InternalConnectionState {
      internalConnectionStateHandle :: Maybe IO.Handle,
      internalConnectionStateOutgoingMessageQueue :: Vector.Vector Message
    }


getConnectionHandle :: (MonadConnection m) => m (Maybe IO.Handle)
getConnectionHandle = do
  state <- getConnectionState
  return $ internalConnectionStateHandle state


setConnectionHandle :: (MonadConnection m) => Maybe IO.Handle -> m ()
setConnectionHandle handle = do
  state <- getConnectionState
  setConnectionState $ state { internalConnectionStateHandle = handle }


enqueueOutgoingMessage :: (MonadConnection m) => Message -> m ()
enqueueOutgoingMessage message = do
  state <- getConnectionState
  let oldQueue = internalConnectionStateOutgoingMessageQueue state
      newQueue = Vector.concat [oldQueue, Vector.singleton message]
  setConnectionState $ state {
      internalConnectionStateOutgoingMessageQueue = newQueue
    }


takeOutgoingMessageQueue :: (MonadConnection m) => m (Vector.Vector Message)
takeOutgoingMessageQueue = do
  state <- getConnectionState
  let queue = internalConnectionStateOutgoingMessageQueue state
  setConnectionState $ state {
      internalConnectionStateOutgoingMessageQueue = Vector.empty
    }
  return queue


dropOutgoingMessageQueue :: (MonadConnection m) => m ()
dropOutgoingMessageQueue = do
  _ <- takeOutgoingMessageQueue
  return ()


newtype ConnectionT m a =
  ConnectionT {
      unConnectionT :: ConnectionState -> m (ConnectionState, a)
    }
instance (Monad m) => Monad (ConnectionT m) where
  return v = ConnectionT $ \state -> return (state, v)
  a >>= b = ConnectionT $ \state -> do
    (state, v) <- unConnectionT a state
    unConnectionT (b v) state
instance (Monad m, Functor m) => Functor (ConnectionT m) where
  fmap f a = ConnectionT $ \state -> do
    (state, v) <- unConnectionT a state
    return (state, f v)
instance (Monad m, Applicative m) => Applicative (ConnectionT m) where
  pure v = return v
  a <*> b = ap a b
instance (Monad m) => MonadConnection (ConnectionT m) where
  getConnectionState = ConnectionT $ \state -> return (state, state)
  setConnectionState state = ConnectionT $ \_ -> return (state, ())
instance MonadTrans ConnectionT where
  lift action = ConnectionT $ \state -> do
    v <- action
    return (state, v)
instance MonadTransControl ConnectionT where
  newtype StT ConnectionT a = StConnectionT {
      unStConnectionT :: (ConnectionState, a)
    }
  liftWith action = do
    state <- getConnectionState
    lift $ action $ \runFromParent -> runConnectionT $ do
      setConnectionState state
      result <- runFromParent
      state <- getConnectionState
      return $ StConnectionT (state, result)
  restoreT action = do
    stConnectionT <- lift action
    let (state, result) = unStConnectionT stConnectionT
    setConnectionState state
    return result
instance (MonadIO m) => MonadIO (ConnectionT m) where
  liftIO action = lift $ liftIO action
instance (MonadBase IO m) => MonadBase IO (ConnectionT m) where
  liftBase action = lift $ liftBase action
instance (MonadBaseControl IO m) => MonadBaseControl IO (ConnectionT m) where
  newtype StM (ConnectionT m) a = StMConnectionT {
      unStMConnectionT :: ComposeSt ConnectionT m a
    }
  liftBaseWith = defaultLiftBaseWith StMConnectionT
  restoreM = defaultRestoreM unStMConnectionT
instance (MonadThrow m) => MonadThrow (ConnectionT m) where
  monadThrow e = lift $ monadThrow e
instance (MonadLogger m) => MonadLogger (ConnectionT m) where
  monadLoggerLog location source level message =
    lift $ monadLoggerLog location source level message


runConnectionT :: (Monad m) => ConnectionT m a -> m a
runConnectionT action = do
  (_, v) <- unConnectionT action $ InternalConnectionState {
      internalConnectionStateHandle = Nothing,
      internalConnectionStateOutgoingMessageQueue = Vector.empty
    }
  return v


sourceIRC
  :: (MonadBase IO m, MonadLogger m, MonadIO m, MonadThrow m,
      MonadConnection m)
  => NetworkSettings
  -> Producer m Event
sourceIRC networkSettings = do
  withConnectionForever networkSettings $ \handle -> do
    mvar <- newMVar False
    threadID <- liftBase $ fork $ do
      threadDelay 60000000
      _ <- swapMVar mvar True
      return ()
    setConnectionHandle $ Just handle
    yield ConnectEvent
    takeOutgoingMessageQueue >>= mapM_ send . Vector.toList
    ConduitBinary.sourceHandle handle
      =$= ConduitText.decode ConduitText.utf8
      =$= ConduitText.lines
      =$= ConduitList.map
            (\line ->
               if Text.isSuffixOf "\r" line
                 then Text.take (Text.length line - 1) line
                 else line)
      =$= ConduitList.mapMaybe parseMessage
      =$= ConduitList.map MessageEvent
    killThread threadID
    result <- takeMVar mvar
    setConnectionHandle Nothing
    yield DisconnectEvent
    return result


send
  :: (MonadBase IO m, MonadLogger m, MonadConnection m)
  => Message
  -> m ()
send message = do
  maybeHandle <- getConnectionHandle
  case maybeHandle of
    Just handle -> do
      let line = formatMessage message
          byteString = Text.encodeUtf8 $ Text.concat [line, "\r\n"]
      liftBase $ ByteString.hPut handle byteString
      liftBase $ IO.hFlush handle
    Nothing -> enqueueOutgoingMessage message


withConnectionForever
  :: (MonadBase IO m, MonadLogger m, MonadConnection m)
  => NetworkSettings
  -> (IO.Handle -> m Bool)
  -> m ()
withConnectionForever networkSettings action = do
  let loop nFailures = do
        maybeSocket <- newConnection networkSettings
        case maybeSocket of
          Nothing -> return ()
          Just socket -> do
            handle <- liftBase $ Network.socketToHandle socket IO.ReadWriteMode
            terminatedNormally <- action handle
            if terminatedNormally
              then loop 0
              else do
                exponentTwiddle <- liftBase $ Random.randomRIO (0, 100)
                let exponent =
                      1.25 + fromIntegral (exponentTwiddle - 50 :: Int) / 100.0
                    delay = floor $ 1000000.0 *
                      ((0.5 ** (fromIntegral nFailures * negate exponent))
                       - 1.0 :: Double)
                $(logInfo) (Text.concat
                  ["Abnormal disconnection from the network ",
                   networkSettingsName networkSettings,
                   "; pausing attempts for ",
                   Text.pack $ show $ fromIntegral delay / 1000000.0,
                   " seconds..."])
                liftBase $ threadDelay delay
                loop (nFailures + 1)
  loop 0


newConnection
  :: (MonadBase IO m, MonadLogger m)
  => NetworkSettings
  -> m (Maybe Network.Socket)
newConnection networkSettings = do
  let networkName = networkSettingsName networkSettings
  case networkSettingsServers networkSettings of
    [] -> do
      $(logError) (Text.concat
        ["No servers configured for the network ", networkName, "."])
      return Nothing
    allServerSettings -> do
      let retryLoop
            :: (MonadBase IO m, MonadLogger m)
            => [ServerSettings]
            -> m (Maybe Network.Socket)
          retryLoop allServerSettings = do
            allServerSettings <- shuffle allServerSettings
            maybeSocket <- eachServerLoop allServerSettings
            case maybeSocket of
              Nothing -> do
                $(logError) (Text.concat
                   ["Failed to connect to any server for the network ",
                    networkName,
                    "; pausing attempts for 30 seconds..."])
                threadDelay 30000
                retryLoop allServerSettings
              Just socket -> return $ Just socket
          eachServerLoop
            :: (MonadBase IO m, MonadLogger m)
            => [ServerSettings]
            -> m (Maybe Network.Socket)
          eachServerLoop [] = return Nothing
          eachServerLoop (serverSettings : restServerSettings) = do
            let hostname = serverSettingsHost serverSettings
            maybeAddrInfo <- lookUpServer hostname
            case maybeAddrInfo of
              Nothing -> eachServerLoop restServerSettings
              Just addrInfo -> do
                maybeSocket <-
                  eachPortLoop hostname addrInfo
                    (Set.toList $ serverSettingsPorts serverSettings)
                case maybeSocket of
                  Nothing -> do
                    eachServerLoop restServerSettings
                  Just socket -> return $ Just socket
          eachPortLoop
            :: (MonadBase IO m, MonadLogger m)
            => Text.Text
            -> Network.AddrInfo
            -> [Int]
            -> m (Maybe Network.Socket)
          eachPortLoop _ _ [] = return Nothing
          eachPortLoop hostname addrInfo (port : restPorts) = do
            maybeSocket <- connectToServer networkName hostname addrInfo port
            case maybeSocket of
              Nothing -> eachPortLoop hostname addrInfo restPorts
              Just socket -> return $ Just socket
      retryLoop allServerSettings


shuffle :: (MonadBase IO m) => [a] -> m [a]
shuffle items = do
  case items of
    [] -> return []
    _ -> do
      index <- liftBase $ Random.randomRIO (0, List.length items - 1)
      let item = List.head $ List.drop index items
          rest = List.take index items ++ List.drop (index + 1) items
      shuffledRest <- shuffle rest
      return $ item : shuffledRest


lookUpServer
  :: (MonadBase IO m, MonadLogger m)
  => Text.Text
  -> m (Maybe Network.AddrInfo)
lookUpServer hostname = do
  $(logInfo) (Text.concat ["Resolving ", hostname, "..."])
  let hints = Network.defaultHints {
                  Network.addrFlags = [],
                  Network.addrFamily = Network.AF_UNSPEC,
                  Network.addrSocketType = Network.Stream,
                  Network.addrProtocol = 6
                }
  results <- liftBase $ catch
    (Network.getAddrInfo (Just hints) (Just $ Text.unpack hostname) Nothing)
    (\e -> do
       return (e :: SomeException)
       return [])
  case results of
    [] -> do
      $(logError) (Text.concat ["Unable to resolve ", hostname, "."])
      return Nothing
    (result : _) -> do
      $(logInfo) (Text.concat
         ["Resolved ", hostname, " to: ", Text.pack $ show result])
      return $ Just result


connectToServer
  :: (MonadBase IO m, MonadLogger m)
  => Text.Text
  -> Text.Text
  -> Network.AddrInfo
  -> Int
  -> m (Maybe Network.Socket)
connectToServer networkName hostname addrInfo port = do
  let address = addressWithPort (Network.addrAddress addrInfo)
                                (fromIntegral port)
      family = Network.addrFamily addrInfo
      socketType = Network.addrSocketType addrInfo
      protocol = Network.addrProtocol addrInfo
  $(logInfo) (Text.concat
    ["Attempting connection to ",
     Text.pack $ show address,
     " port ",
     Text.pack $ show port,
     " for ",
     hostname,
     " in the network ",
     networkName,
     "."])
  maybeSocket <- liftBase $ catch
    (Network.socket family socketType protocol >>= return . Just)
    (\e -> do
       return (e :: SomeException)
       return Nothing)
  success <- case maybeSocket of
    Just socket -> do
      success <- liftBase $ catch
        (Network.connect socket address >> return True)
        (\e -> do
           return (e :: SomeException)
           return False)
      if success
        then do
          $(logInfo) (Text.concat
             ["Connected to ",
              Text.pack $ show address,
              " port ",
              Text.pack $ show port,
              " for ",
              hostname,
              " in the network ",
              networkName,
              "."])
          return True
        else do
          liftBase $ catch
            (Network.close socket)
            (\e -> do
              return (e :: SomeException)
              return ())
          return False
    Nothing -> return False
  if success
    then return ()
    else do
      $(logError) (Text.concat
        ["Failed to connect to ",
         Text.pack $ show address,
         " port ",
         Text.pack $ show port,
         " for ",
         hostname,
         " in the network ",
         networkName,
         "."])
  return maybeSocket


addressWithPort :: Network.SockAddr -> Network.PortNumber -> Network.SockAddr
addressWithPort (Network.SockAddrInet _ hostAddress) port =
  Network.SockAddrInet port hostAddress
addressWithPort (Network.SockAddrInet6 _ flowInfo hostAddress6 scopeID) port =
  Network.SockAddrInet6 port flowInfo hostAddress6 scopeID
addressWithPort other _ = other


{-
      receiveForever :: IO ()
      receiveForever = do
        let stateMVar = networkConnectionStateMVar connection
        state <- readMVar stateMVar
        let socket = networkConnectionStateSocket state
            receiveForever' buffer = do
              maybeBuffer <- moreBuffer socket buffer
              case maybeBuffer of
                Nothing -> do
                  _ <- swapMVar stateMVar UnconnectedNetworkConnectionState
                  return ()
                Just buffer -> do
                  buffer <- consumeBuffer buffer
                  receiveForever' buffer
        receiveForever' BS.empty
      
      moreBuffer :: Socket -> ByteString -> IO (Maybe ByteString)
      moreBuffer socket oldBuffer = do
        newInput <- recv socket 1024
        if BS.null newInput
          then do
            sClose socket
            return Nothing
          else return $ Just $ BS.append oldBuffer newInput
      
      consumeBuffer :: ByteString -> IO ByteString
      consumeBuffer buffer = do
        let (maybeLine, restBuffer) = takeLine buffer
        case maybeLine of
          Nothing -> return restBuffer
          Just line -> do
            processReceivedLine line
            consumeBuffer restBuffer
      
      takeLine :: ByteString -> (Maybe ByteString, ByteString)
      takeLine buffer =
        let (line, afterLine) =
              BS.breakSubstring (UTF8.fromString "\r\n") buffer
        in if BS.null afterLine
             then (Nothing, buffer)
             else let rest = BS.drop 2 afterLine
                  in (Just line, rest)
      
      processReceivedLine :: ByteString -> IO ()
      processReceivedLine line = do
        let maybeMessage = parseMessage $ UTF8.toString line
        case maybeMessage of
          Nothing -> return ()
          Just message -> do
            triggers <- readMVar $ connectionTriggersMVar connection
            foundMatch <- foldM (\foundMatch trigger -> do
                                   if foundMatch
                                     then return True
                                     else do
                                       let pattern =
                                             triggerMessagePattern trigger
                                           matches =
                                             matchMessage message pattern
                                           action = triggerAction trigger
                                       if matches
                                         then do
                                           action message
                                           return True
                                         else return False)
                                False
                                triggers
            if not foundMatch
              then connectionStatusMessage
                    connection
                    $ (let maybeSender = messageSender message
                           command = messageCommand message
                           parameters = messageParameters message
                       in show maybeSender ++
                          " "
                          ++ command
                          ++ " "
                          ++ show parameters)
              else return ()
      
      disconnect :: IO ()
      disconnect = do
        connectionStatusMessage
         connection
         "Disconnected."
            
      connectToAddress :: String -> AddrInfo -> Int -> IO Bool
      connectToAddress _ addressInfo port = do
        catch (do
                let address = addressWithPort (addrAddress addressInfo)
                                              (fromIntegral port)
                    family = addrFamily addressInfo
                    socketType = addrSocketType addressInfo
                    protocol = addrProtocol addressInfo
                connectionStatusMessage
                 connection
                 $ "Attempting connection to " ++ (show address) ++ "..."
                socket' <- socket family socketType protocol
                sendMVar <- newMVar ()
                let state = ConnectingNetworkConnectionState {
                                networkConnectionStateSocket = socket',
                                networkConnectionStateSendMVar = sendMVar
                              }
                    stateMVar = networkConnectionStateMVar connection
                _ <- swapMVar stateMVar state
                connect socket' address
                return True)
              (\e -> do
                return (e :: SomeException)
                return False)
      
  attemptConnectionToRandomServer
connectionMain connection@(DirectClientConnection { }) = do
  return ()


connectionLogIn :: Connection -> IO ()
connectionLogIn connection@(NetworkConnection { }) = do
  let networkIdentifier = networkConnectionNetworkIdentifier connection
      applicationStateMVar = connectionApplicationStateMVar connection
      triggersMVar = connectionTriggersMVar connection
  (nickname, realName)
    <- flip runApplication applicationStateMVar $ do
         nickname <- getNicknameForNetwork networkIdentifier
         realName <- getRealNameForNetwork networkIdentifier
         return (nickname, realName)
  let sendLogInAttempt :: Maybe String -> IO ()
      sendLogInAttempt maybeRandomSuffix = do
        suffixedNickname <- case maybeRandomSuffix of
                              Nothing -> return nickname
                              Just randomSuffix ->
                                return $ nickname ++ randomSuffix
        sendMessage
         connection
         $ Message {
               messageSender = Nothing,
               messageCommand = "NICK",
               messageParameters = [suffixedNickname]
             }
      sendInitialLogInAttempt :: IO ()
      sendInitialLogInAttempt = sendLogInAttempt Nothing
      sendAdditionalLogInAttempt :: IO ()
      sendAdditionalLogInAttempt = do
        randomQuantity <- randomRIO (1, 999) :: IO Int
        let randomSuffixUnpadded = show randomQuantity
            randomSuffix = (take (3 - length randomSuffixUnpadded)
                                 (repeat '0'))
                           ++ randomSuffixUnpadded
        sendLogInAttempt $ Just randomSuffix
      completeLogIn :: String -> IO ()
      completeLogIn actualNickname = do
        let stateMVar = networkConnectionStateMVar connection
        oldState <- takeMVar stateMVar
        case oldState of
          ConnectingNetworkConnectionState { } -> do
            let newlyConnectedState =
                  ConnectedNetworkConnectionState {
                      networkConnectionStateSocket =
                        networkConnectionStateSocket oldState,
                      networkConnectionStateSendMVar =
                        networkConnectionStateSendMVar oldState,
                      connectedNetworkConnectionStateNickname =
                        actualNickname
                    }
            putMVar stateMVar newlyConnectedState
          _ -> putMVar stateMVar oldState
  addResponseTriggers
   connection
   [(MessagePattern {
         messagePatternCommand = "433"
       },
     \_ _ -> sendAdditionalLogInAttempt),
    (MessagePattern {
         messagePatternCommand = "001"
       },
     \triggerIdentifiers message -> do
       removeTriggers connection triggerIdentifiers
       let parameters = messageParameters message
       if length parameters > 0
         then completeLogIn $ parameters !! 0
         else completeLogIn nickname)]
   $ do
       sendInitialLogInAttempt
       sendMessage
        connection
        $ Message {
              messageSender = Nothing,
              messageCommand = "USER",
              messageParameters = [nickname, "*", "*", realName]
            }
connectionLogIn connection@(DirectClientConnection { }) = do
  return ()


initialTriggers :: Connection -> [(MessagePattern, Message -> IO ())]
initialTriggers connection =
  let ignore command =
        (MessagePattern {
             messagePatternCommand = command
           },
         \_ -> return ())
      toStatus command parameterIndex =
        (MessagePattern {
             messagePatternCommand = command
           },
         \message -> do
           let parameters = messageParameters message
           if length parameters > parameterIndex
             then connectionStatusMessage
                   connection
                   $ parameters !! parameterIndex
             else return ())
      otherHandler command handler =
        (MessagePattern {
             messagePatternCommand = command
           },
         \message -> handler connection message)
  in [ignore "001",
      ignore "002",
      ignore "003",
      ignore "004",
      ignore "005",
      ignore "251",
      ignore "252",
      ignore "253",
      ignore "254",
      ignore "255",
      ignore "265",
      ignore "266",
      toStatus "375" 1,
      toStatus "372" 1,
      toStatus "376" 1,
      toStatus "ERROR" 0,
      otherHandler "PING" handlePingReceived,
      otherHandler "PRIVMSG" handlePrivmsgReceived,
      otherHandler "NOTICE" handleNoticeReceived,
      otherHandler "332" handleChannelTopicReceived,
      otherHandler "333" handleChannelTopicLastSetReceived,
      otherHandler "353" handleChannelNamesReceived,
      otherHandler "366" handleChannelNamesEndReceived]



handlePingReceived :: Connection -> Message -> IO ()
handlePingReceived connection message = do
  sendMessage
   connection
   $ Message {
         messageSender = Nothing,
         messageCommand = "PONG",
         messageParameters = messageParameters message
       }


handlePrivmsgReceived :: Connection -> Message -> IO ()
handlePrivmsgReceived connection message = do
  channelTabMap <- readMVar $ networkConnectionChannelTabMapMVar connection
  let parameters = messageParameters message
      recipient = if length parameters > 0
                    then parameters !! 0
                    else ""
      text = if length parameters > 1
               then parameters !! 1
               else ""
      nickname = messageSenderNickname message
      maybeChannelTab = Map.lookup recipient channelTabMap
  case maybeChannelTab of
    Nothing -> return ()
    Just tab -> do
      flip runApplication connection $ do
        appendContent tab
                      $ Content {
                            contentNickname = nickname,
                            contentBody = text
                          }


handleNoticeReceived :: Connection -> Message -> IO ()
handleNoticeReceived connection message = do
  let parameters = messageParameters message
  if length parameters > 1
    then connectionStatusMessage
          connection
          $ parameters !! 1
    else return ()


handleChannelTopicReceived :: Connection -> Message -> IO ()
handleChannelTopicReceived connection message = do
  putStrLn $ messageCommand message ++ " " ++ (show $ messageParameters message)
  return ()
  -- 332 irene-lew130 #xyzzy :Foo!


handleChannelTopicLastSetReceived :: Connection -> Message -> IO ()
handleChannelTopicLastSetReceived connection message = do
  putStrLn $ messageCommand message ++ " " ++ (show $ messageParameters message)
  return ()
  -- 333 irene-lew130 #xyzzy irene-lew 1317757155


handleChannelNamesReceived :: Connection -> Message -> IO ()
handleChannelNamesReceived connection message = do
  putStrLn $ messageCommand message ++ " " ++ (show $ messageParameters message)
  return ()
  -- 353 irene-lew130 - #xyzzy :irene-lew130 @irene-lew


handleChannelNamesEndReceived :: Connection -> Message -> IO ()
handleChannelNamesEndReceived connection message = do
  putStrLn $ messageCommand message ++ " " ++ (show $ messageParameters message)
  return ()
  -- 366 irene-lew130 #xyzzy :End of /NAMES list.

-}
