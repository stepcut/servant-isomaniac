{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}

-- #if !MIN_VERSION_base(4,8,0)
-- {-# LANGUAGE OverlappingInstances #-}
-- #endif
module Servant.Isomaniac where

-- #if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
-- #endif
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy       (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Functor.Identity (Identity(..))
import qualified Data.Text.Encoding as Text
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text                  (Text, unpack, pack)
import qualified Data.JSString as JS
import           GHC.TypeLits
import           GHCJS.Buffer               (toByteString, fromByteString, getArrayBuffer, createFromArrayBuffer)
import qualified GHCJS.Buffer as Buffer

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal (FromJSVal(..))
import           GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))
import JavaScript.Cast (unsafeCast)
import GHCJS.Types (JSVal(..), isNull, isUndefined)
import GHCJS.Buffer (freeze)
import JavaScript.Web.MessageEvent as WebSockets
import JavaScript.Web.WebSocket (WebSocket, WebSocketRequest(..))
-- import JavaScript.TypedArray.ArrayBuffer.Internal
import qualified JavaScript.Web.WebSocket as WebSockets
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, MutableArrayBuffer)
import qualified JavaScript.TypedArray.ArrayBuffer as ArrayBuffer
-- import GHCJS.Buffer (SomeArrayBuffer(..))
-- import           Network.HTTP.Isomaniac        (Response, Manager)
-- import           Network.HTTP.Media
-- import qualified Network.HTTP.Types         as H
-- import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.API hiding (getResponse)
import           Servant.Common.BaseUrl
import           Servant.Common.Req
import Network.HTTP.Media (renderHeader)
import Web.HttpApiData
-- import JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import Web.ISO.Diff
import Web.ISO.Patch
import Web.ISO.Types
import GHC.Exts (State#)

data ReqAction action
  = ReqAction Req (B.ByteString -> Maybe action)

instance Functor ReqAction where
    fmap f (ReqAction req decoder) = ReqAction req (\json -> fmap f (decoder json))

data MUV m model ioData action remote = MUV
    { model     :: model
    , browserIO :: TQueue action -> action -> model -> IO ioData
    , update    :: action -> ioData -> model -> (model, Maybe remote)
    , view      :: model  -> (HTML action, [Canvas])
    }

class HasIsomaniac layout where
  type Isomaniac layout :: *
  isomaniacWithRoute :: Proxy layout -> Req -> BaseUrl -> Isomaniac layout

isomaniac :: (HasIsomaniac layout) => Proxy layout -> BaseUrl -> Isomaniac layout
isomaniac p baseURL = isomaniacWithRoute p defReq baseURL

instance (HasIsomaniac a, HasIsomaniac b) => HasIsomaniac (a :<|> b) where
  type Isomaniac (a :<|> b) = Isomaniac a :<|> Isomaniac b
  isomaniacWithRoute Proxy req baseurl =
    isomaniacWithRoute (Proxy :: Proxy a) req baseurl :<|>
    isomaniacWithRoute (Proxy :: Proxy b) req baseurl

instance (KnownSymbol capture, ToHttpApiData a, HasIsomaniac sublayout)
      => HasIsomaniac (Capture capture a :> sublayout) where

  type Isomaniac (Capture capture a :> sublayout) =
    a -> Isomaniac sublayout

  isomaniacWithRoute Proxy req baseurl val =
    isomaniacWithRoute (Proxy :: Proxy sublayout)
                    (appendToPath p req)
                    baseurl

    where p = toUrlPiece val

-- | If you have a 'Put' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
         {-# OVERLAPPABLE #-}
  (FromJSON a, MimeUnrender ct a) => HasIsomaniac (Put (ct ': cts) a) where
  type Isomaniac (Put (ct ': cts) a) = ReqAction a
  isomaniacWithRoute Proxy req baseurl =
      ReqAction (req { method = "POST" }) decodeStrict

instance
         {-# OVERLAPPING #-}
  HasIsomaniac (Put (ct ': cts) ()) where
  type Isomaniac (Put (ct ': cts) ()) = ReqAction ()
  isomaniacWithRoute Proxy req baseurl =
      ReqAction (req { method = "POST" }) (const (Just ()))
{-
-- | If you have a 'Put xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
         {-# OVERLAPPING #-}
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasIsomaniac (Put (ct ': cts) (Headers ls a)) where
  type Isomaniac (Put (ct ': cts) (Headers ls a)) = Req
  clientWithRoute Proxy req baseurl manager= do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPut req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

-}
instance {-# OVERLAPPABLE #-}
  (FromJSON a, MimeUnrender ct a, cts' ~ (ct ': cts)) => HasIsomaniac (Delete cts' a) where
  type Isomaniac (Delete cts' a) = ReqAction a
  isomaniacWithRoute Proxy req baseurl =
    ReqAction (req { method = "DELETE" }) decodeStrict

instance {-# OVERLAPPING #-}
  HasIsomaniac (Delete cts ()) where
  type Isomaniac (Delete cts ()) = ReqAction ()
  isomaniacWithRoute Proxy req baseurl =
    ReqAction (req { method = "DELETE" }) (const $ Just ())

{-
-- | If you have a 'Delete xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
  {-# OVERLAPPING #-}
  ( FromJSON a, MimeUnrender ct a, BuildHeadersTo ls, cts' ~ (ct ': cts)
  ) => HasIsomaniac (Delete cts' (Headers ls a)) where
  type Isomaniac (Delete cts' (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  isomaniacWithRoute Proxy req baseurl manager = do

    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodDelete req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }
-}

{-
instance (KnownSymbol sym, ToText a, HasIsomaniac sublayout)
      => HasIsomaniac (Header sym a :> sublayout) where

  type Isomaniac (Header sym a :> sublayout) =
    Maybe a -> Isomaniac sublayout

  isomaniacWithRoute Proxy req baseurl mval =
    isomaniacWithRoute (Proxy :: Proxy sublayout)
                    (maybe req
                           (\value -> Servant.Common.Req.addHeader hname value req)
                           mval
                    )
                    baseurl
                    manager

    where hname = symbolVal (Proxy :: Proxy sym)
-}
instance {-# OVERLAPPABLE #-}
  (FromJSON result, MimeUnrender ct result) => HasIsomaniac (Get (ct ': cts) result) where
  type Isomaniac (Get (ct ': cts) result) = ReqAction result
  isomaniacWithRoute Proxy req baseurl = ReqAction (req { method = "GET" }) decodeStrict
--    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodGet req baseurl manager

-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> ExceptT String IO Book
-- > addBook = client myApi host manager
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "addBook" to query that endpoint
instance (MimeRender ct a, HasIsomaniac sublayout)
      => HasIsomaniac (ReqBody (ct ': cts) a :> sublayout) where

  type Isomaniac (ReqBody (ct ': cts) a :> sublayout) =
    a -> Isomaniac sublayout

  isomaniacWithRoute Proxy req baseurl body =
          isomaniacWithRoute (Proxy :: Proxy sublayout)
              (let ctProxy = Proxy :: Proxy ct
                   bdy = mimeRender ctProxy body
                   ct  = contentType ctProxy
               in req { reqBody = Just (bdy, ct)
                      {- , headers = ("Content-Type", Text.decodeUtf8 $ renderHeader ct) : (headers req) -}
                      }) baseurl

{-
    isomaniacWithRoute (Proxy :: Proxy sublayout)
                    (let ctProxy = Proxy :: Proxy ct
                     in setRQBody (mimeRender ctProxy body)
                                  (contentType ctProxy)
                                  req
                    )
                    baseurl manager
-}
-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasIsomaniac sublayout) => HasIsomaniac (path :> sublayout) where
  type Isomaniac (path :> sublayout) = Isomaniac sublayout

  isomaniacWithRoute Proxy req =
     isomaniacWithRoute (Proxy :: Proxy sublayout) $
       appendToPath p req

    where p = pack $ symbolVal (Proxy :: Proxy path)


-- | If you have a 'Post' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
         {-# OVERLAPPABLE #-}
  (FromJSON a, MimeUnrender ct a) => HasIsomaniac (Post (ct ': cts) a) where
  type Isomaniac (Post (ct ': cts) a) = ReqAction a
  isomaniacWithRoute Proxy req baseurl = ReqAction (req { method = "POST" }) decodeStrict
{-
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPost req baseurl manager
-}
instance
         {-# OVERLAPPING #-}
  HasIsomaniac (Post (ct ': cts) ()) where
  type Isomaniac (Post (ct ': cts) ()) = ReqAction ()
  isomaniacWithRoute Proxy req baseurl =
      ReqAction (req { method = "POST" }) (const (Just ()))
{-
    void $ performRequestNoBody H.methodPost req baseurl manager
-}

-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToHttpApiData a, HasIsomaniac sublayout)
      => HasIsomaniac (QueryParam sym a :> sublayout) where

  type Isomaniac (QueryParam sym a :> sublayout) =
    Maybe a -> Isomaniac sublayout

  -- if mparam = Nothing, we don't add it to the query string
  isomaniacWithRoute Proxy req baseurl mparam =
    isomaniacWithRoute (Proxy :: Proxy sublayout) req -- FIMXE actually add the query params
                       {-
                    (maybe req
                           (flip (appendToQueryString pname) req . Just)
                           mparamText
                    ) -}
                    baseurl

{-
    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toQueryParam mparam
-}
mainLoopRemote :: (Show action) =>
--               -> (Text -> action)
                (TQueue action -> IO (remote -> IO ()))
               -> JSDocument
               -> JSNode
               -> MUV m model ioData action remote -- (ReqAction action)
--               -> (forall a. m a -> IO a)
               -> Maybe action
               -> IO ()
mainLoopRemote initRemote document body (MUV model calcIoData update view) {- runM -} mInitAction =
    do queue <- atomically newTQueue
       let (vdom, canvases) = view model
       -- update HTML
       html <- renderHTML (handleAction queue) document vdom
       removeChildren body
       appendChild body html
       -- update Canvases
       mapM_ drawCanvas canvases
       w <- window
--       addEventListener w KeyDown (\e -> js_alert (JS.pack (show (keyCode e))) >> defaultPrevented e >>= \b -> js_alert (JS.pack (show b)) >> preventDefault e >> defaultPrevented e >>= \b -> js_alert (JS.pack (show b)) ) False
--       addEventListener document KeyUp (\e -> defaultPrevented e >>= \b -> js_alert (JS.pack (show b)) >> preventDefault e >> defaultPrevented e >>= \b -> js_alert (JS.pack (show b)) ) False

--       remoteLoop queue xhr
       handleRemote <- initRemote queue
       case mInitAction of
         (Just initAction) ->
             handleAction queue initAction
         Nothing -> pure ()
       loop handleRemote {- xhr -} queue {- decodeVar -} model vdom
    where

      handleAction queue = \action -> atomically $ writeTQueue queue action
--      remoteLoop queue xhr = forkIO $
--          return ()
      loop handleRemote {- xhr -} queue {- decodeVar -} model oldVDom =
          do action <- atomically $ readTQueue queue
             ioData <- calcIoData queue action model
             let (model', mremote') = update action ioData model
             let (vdom, canvases)   = view model'
                 diffs = diff oldVDom (Just vdom)
--             putStrLn $ "action --> " ++ show action
--             putStrLn $ "diff --> " ++ show diffs
             -- update HTML
             apply (handleAction queue) document body oldVDom diffs
             -- update Canvases
             mapM_ drawCanvas canvases
--             html <- renderHTML (handleAction queue) document vdom
--             removeChildren body
--             appendJSChild body html
             case mremote' of
               Nothing -> pure ()
               (Just remote) -> handleRemote remote
{-
               (Just (ReqAction req decoder)) ->
                   do atomically $ putTMVar decodeVar decoder
                      open xhr (method req) ({- "http://localhost:8000" <> -} reqPath req) True
                      setResponseType xhr "arraybuffer" -- FIXME: do we need to do this everytime?
                      setRequestHeader xhr "Accept" "application/json" -- FIXME, use reqAccept
                      mapM_ (\(h, v) -> setRequestHeader xhr h v) (headers req)
                      case reqBody req of
                        Nothing -> send xhr
                        (Just (bdy, ct)) ->
                            do setRequestHeader xhr "Content-Type" (Text.decodeUtf8 $ renderHeader ct)
                               let (buffer, _, _) = (fromByteString $ toStrict bdy)
                               sendArrayBuffer xhr buffer
                      print "xhr sent."
--                      mapM_ (\a -> reqAccept (setRequestHeader "Accept"
                      -- FIXME: QueryString
-}
{-
               (Just remote) ->
                   do open xhr "POST" url True
                      sendString xhr (textToJSString remote)
-}
             loop handleRemote {- xhr -} queue {- decodeVar -} model' vdom
{-
foreign import javascript unsafe
  "$2.slice($1)" js_slice1_imm :: Int -> SomeArrayBuffer any -> SomeArrayBuffer any
foreign import javascript unsafe
  "$3.slice($1,$2)" js_slice_imm :: Int -> Int -> SomeArrayBuffer any -> SomeArrayBuffer any

slice :: Int -> Maybe Int -> SomeArrayBuffer any -> SomeArrayBuffer any
slice begin (Just end) b = js_slice_imm begin end b
slice begin _          b = js_slice1_imm begin b

{-# INLINE slice #-}
-}
sendRemoteWS :: (ToJSON remote) => WebSocket -> remote -> IO ()
sendRemoteWS ws remote =
  do let jstr = JS.pack (C.unpack $ encode remote)
     WebSockets.send jstr ws

foreign import javascript unsafe  "console[\"log\"]($1)" consoleLog :: JS.JSString -> IO ()

foreign import javascript unsafe "function(buf){ if (!buf) { throw \"checkArrayBuffer: !buf\"; }; if (!(buf instanceof ArrayBuffer)) { throw \"checkArrayBuffer: buf is not an ArrayBuffer\"; }}($1)" checkArrayBuffer :: MutableArrayBuffer -> IO ()
-- if(!buf || !(buf instanceof ArrayBuffer))
--    throw "h$wrapBuffer: not an ArrayBuffer"

-- foreign import javascript unsafe
--   "new ArrayBuffer($1)" js_create :: Int -> State# s -> (# State# s, JSVal #)

foreign import javascript unsafe
   "new ArrayBuffer($1)" js_create :: Int -> IO MutableArrayBuffer

create :: Int -> IO MutableArrayBuffer
create n = js_create n
{-# INLINE create #-}

logMessage :: MessageEvent -> IO ()
logMessage messageEvent =
  case WebSockets.getData messageEvent of
    (StringData str)    -> consoleLog str
    (ArrayBufferData r) -> do consoleLog "Got ArrayBufferData"
                              marray <- ArrayBuffer.thaw r
                              let jsval = (pToJSVal marray)
                                  buf   = createFromArrayBuffer r :: Buffer.Buffer
                              ab <- create 10
                              checkArrayBuffer marray
                              consoleLog ("checkArrayBuffer passed.")
                              consoleLog (JS.pack (show (isUndefined jsval)))
                              consoleLog (JS.pack (show (isNull jsval)))
                              consoleLog (JS.pack (show (toByteString 0 Nothing buf)))
                              -- -- (show ((decodeStrict (toByteString 0 Nothing (createFromArrayBuffer r))) :: Maybe WebSocketRes)))

incomingWS :: (MessageEvent -> Maybe action) -> TQueue action -> MessageEvent -> IO ()
incomingWS decodeAction queue messageEvent =
  do logMessage messageEvent
     case decodeAction messageEvent of
       Nothing -> consoleLog "Failed to decode messageEvent"
       (Just action) -> atomically $ writeTQueue queue action

initRemoteWS :: (ToJSON remote) => JS.JSString -> (MessageEvent -> Maybe action) -> TQueue action -> IO (remote -> IO ())
initRemoteWS url' decodeAction queue =
  do let request = WebSocketRequest { url       = url'
                                    , protocols = []
                                    , onClose   = Nothing
                                    , onMessage = Just (incomingWS decodeAction queue)
                                    }
     ws <- WebSockets.connect request
     pure (sendRemoteWS ws)

initRemoteXHR :: TQueue action -> IO (ReqAction action -> IO ())
initRemoteXHR queue =
  do decodeVar <- atomically newEmptyTMVar
     -- xhr request
     xhr <- newXMLHttpRequest
     -- cb <- asyncCallback (handleXHR queue decodeVar xhr)
     addEventListener xhr ProgressLoad (\e -> handleXHR queue decodeVar xhr) False
     pure (sendRemoteXHR xhr decodeVar)
  where
        handleXHR queue decodeVar xhr =
          do -- FIXME: check status
             rs <- getReadyState xhr
             putStrLn $ "xhr ready state = " ++ show rs
             status <- getStatus xhr
             putStrLn $ "xhr status = " ++ show status
             case rs of
               4 -> do decode <- atomically $ takeTMVar decodeVar
                       txt <- getResponseType xhr
                       print txt
                       txt <- getStatusText xhr
                       print txt
                       ref <- getResponse xhr
                       if isNull ref
                        then print "response was null."
                        else pure ()
                       buf <- Buffer.createFromArrayBuffer <$> (ArrayBuffer.unsafeFreeze $ pFromJSVal ref)
                       let bs = toByteString 0 Nothing buf
                       case decode bs of
                         Nothing -> return ()
                         (Just action) ->
                             atomically $ writeTQueue queue action
               _ -> pure ()

sendRemoteXHR :: XMLHttpRequest -> TMVar (B.ByteString -> Maybe action) -> ReqAction action -> IO ()
sendRemoteXHR xhr decodeVar (ReqAction req decoder) =
  do atomically $ putTMVar decodeVar decoder
     open xhr (method req) ({- "http://localhost:8000" <> -} reqPath req) True
     setResponseType xhr "arraybuffer" -- FIXME: do we need to do this everytime?
     setRequestHeader xhr "Accept" "application/json" -- FIXME, use reqAccept
     mapM_ (\(h, v) -> setRequestHeader xhr h v) (headers req)
     case reqBody req of
       Nothing -> send xhr
       (Just (bdy, ct)) ->
         do setRequestHeader xhr "Content-Type" (Text.decodeUtf8 $ renderHeader ct)
            let (buffer, _, _) = (fromByteString $ toStrict bdy)
            sendArrayBuffer xhr buffer
            print "xhr sent."
--                      mapM_ (\a -> reqAccept (setRequestHeader "Accept"
                      -- FIXME: QueryString


muv :: (Show action) =>
       MUV m model ioData action (ReqAction action)
--    -> (forall a. m a -> IO a)
    -> Maybe action
    -> IO ()
muv muv {- runM -} initAction =
    do (Just document) <- currentDocument
       murvElem
           <- do mmurv <- getElementById document "murv"
                 case mmurv of
                    (Just murv) -> return (toJSNode murv)
                    Nothing ->
                        do (Just bodyList) <- getElementsByTagName document "body"
                           (Just body)     <- item bodyList 0
                           return body
       mainLoopRemote initRemoteXHR document murvElem muv {- runM -} initAction

muvWS :: (Show action, ToJSON remote) =>
       MUV m model ioData action remote
--    -> (forall a. m a -> IO a)
    -> JS.JSString
    -> (MessageEvent -> Maybe action)
    -> Maybe action
    -> IO ()
muvWS muv {- runM -} url decodeAction initAction =
    do (Just document) <- currentDocument
       murvElem
           <- do mmurv <- getElementById document "murv"
                 case mmurv of
                    (Just murv) -> return (toJSNode murv)
                    Nothing ->
                        do (Just bodyList) <- getElementsByTagName document "body"
                           (Just body)     <- item bodyList 0
                           return body
       mainLoopRemote (initRemoteWS url decodeAction) document murvElem muv {- runM -} initAction

runIdent :: Identity a -> IO a
runIdent = pure . runIdentity
