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
import           Data.ByteString.Lazy       (ByteString, fromStrict)
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text                  (Text, unpack, pack)
import           GHC.TypeLits
import           GHCJS.Buffer               (toByteString)
import           GHCJS.Foreign.Callback
-- import           Network.HTTP.Isomaniac        (Response, Manager)
-- import           Network.HTTP.Media
-- import qualified Network.HTTP.Types         as H
-- import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.API hiding (getResponse)
import           Servant.Common.BaseUrl
import           Servant.Common.Req
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

instance (KnownSymbol capture, ToHttpApiData a, HasIsomaniac sublayout)
      => HasIsomaniac (Capture capture a :> sublayout) where

  type Isomaniac (Capture capture a :> sublayout) =
    a -> Isomaniac sublayout

  isomaniacWithRoute Proxy req baseurl val =
    isomaniacWithRoute (Proxy :: Proxy sublayout)
                    (appendToPath p req)
                    baseurl

    where p = toUrlPiece val

{-
instance (KnownSymbol sym, ToHttpApiData a, HasIsomaniac sublayout)
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


{-
instance {-# OVERLAPPABLE #-}
  (MimeUnrender ct a, cts' ~ (ct ': cts)) => HasIsomaniac (Delete cts' a) where
  type Isomaniac (Delete cts' a) = ExceptT ServantError IO a
  isomaniacWithRoute Proxy req baseurl =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodDelete req baseurl

instance {-# OVERLAPPING #-}
  HasIsomaniac (Delete cts ()) where
  type Isomaniac (Delete cts ()) = ExceptT ServantError IO ()
  isomaniacWithRoute Proxy req baseurl =
    void $ performRequestNoBody H.methodDelete req baseurl
-}

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasIsomaniac sublayout) => HasIsomaniac (path :> sublayout) where
  type Isomaniac (path :> sublayout) = Isomaniac sublayout

  isomaniacWithRoute Proxy req =
     isomaniacWithRoute (Proxy :: Proxy sublayout) $
       appendToPath p req

    where p = pack $ symbolVal (Proxy :: Proxy path)

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
       addEventListener xhr ReadyStateChange cb False

--       remoteLoop queue xhr
       case mInitAction of
         (Just initAction) ->
             handleAction queue initAction
         Nothing -> return ()
       loop xhr queue decodeVar model vdom
    where
      handleXHR queue decodeVar xhr =
          do -- FIXME: check status
             decode <- atomically $ takeTMVar decodeVar
             buf <- getResponse xhr
             let bs = toByteString 0 Nothing buf
             case decode bs of
               Nothing -> return ()
               (Just action) ->
                   atomically $ writeTQueue queue action
--             atomically $ writeTQueue queue (h t)
             return ()

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
                      open xhr (method req) (reqPath req) True
                      mapM_ (\(h, v) -> setRequestHeader xhr h v) (headers req)
                      setResponseType xhr "arraybuffer" -- FIXME: do we need to do this everytime?
                      setRequestHeader xhr "Accept" "application/json" -- FIXME, use reqAccept
                      send xhr
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

