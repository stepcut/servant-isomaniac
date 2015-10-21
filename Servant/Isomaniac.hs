{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy       (ByteString, fromStrict, toStrict)
import qualified Data.Text.Encoding as Text
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text                  (Text, unpack, pack)
import           GHC.TypeLits
import           GHCJS.Buffer               (toByteString, fromByteString)
import qualified GHCJS.Buffer as Buffer

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal (FromJSRef(..))
import           GHCJS.Marshal.Pure (PFromJSRef(..))
import JavaScript.Cast (unsafeCast)
import GHCJS.Types (JSRef(..), isNull)
import GHCJS.Buffer (freeze)
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

import Web.ISO.Diff
import Web.ISO.Patch
import Web.ISO.Types

data ReqAction action = ReqAction Req (B.ByteString -> Maybe action)

instance Functor ReqAction where
    fmap f (ReqAction req decoder) = ReqAction req (\json -> fmap f (decoder json))

data MUV  model action remote = MUV
    { model  :: model
    , update :: action -> model -> (model, Maybe remote)
    , view   :: model  -> HTML action
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

instance (KnownSymbol capture, ToText a, HasIsomaniac sublayout)
      => HasIsomaniac (Capture capture a :> sublayout) where

  type Isomaniac (Capture capture a :> sublayout) =
    a -> Isomaniac sublayout

  isomaniacWithRoute Proxy req baseurl val =
    isomaniacWithRoute (Proxy :: Proxy sublayout)
                    (appendToPath p req)
                    baseurl

    where p = toText val

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
instance (KnownSymbol sym, ToText a, HasIsomaniac sublayout)
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
                  JSDocument
               -> JSNode
               -> MUV model action (ReqAction action)
               -> Maybe action
               -> IO ()
mainLoopRemote document body (MUV model update view) mInitAction =
    do queue <- atomically newTQueue
       let vdom = view model
       html <- renderHTML (handleAction queue) document vdom
       removeChildren body
       appendChild body html

       decodeVar <- atomically newEmptyTMVar
       xhr <- newXMLHttpRequest
       cb <- asyncCallback (handleXHR queue decodeVar xhr)
       addEventListener xhr (EventTxt "load") cb False

--       remoteLoop queue xhr
       case mInitAction of
         (Just initAction) ->
             handleAction queue initAction
         Nothing -> return ()
       loop xhr queue decodeVar model vdom
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
                       buf <- Buffer.createFromArrayBuffer <$> (ArrayBuffer.unsafeFreeze $ pFromJSRef ref)
                       let bs = toByteString 0 Nothing buf
                       case decode bs of
                         Nothing -> return ()
                         (Just action) ->
                             atomically $ writeTQueue queue action
               _ -> pure ()

      handleAction queue = \action -> atomically $ writeTQueue queue action
--      remoteLoop queue xhr = forkIO $
--          return ()
      loop xhr queue decodeVar model oldVDom =
          do action <- atomically $ readTQueue queue
             let (model', mremote') = update action model
             let vdom = view model'
                 diffs = diff oldVDom vdom
--             putStrLn $ "action --> " ++ show action
--             putStrLn $ "diff --> " ++ show diffs
             apply (handleAction queue) document body oldVDom diffs
--             html <- renderHTML (handleAction queue) document vdom
--             removeChildren body
--             appendJSChild body html
             case mremote' of
               Nothing -> return ()
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
{-
               (Just remote) ->
                   do open xhr "POST" url True
                      sendString xhr (textToJSString remote)
-}
             loop xhr queue decodeVar model' vdom

muv :: (Show action) =>
       MUV model action (ReqAction action)
    -> Maybe action
    -> IO ()
muv muv initAction =
    do (Just document) <- currentDocument
       murvElem
           <- do mmurv <- getElementById document "murv"
                 case mmurv of
                    (Just murv) -> return (toJSNode murv)
                    Nothing ->
                        do (Just bodyList) <- getElementsByTagName document "body"
                           (Just body)     <- item bodyList 0
                           return body
       mainLoopRemote document murvElem muv initAction
