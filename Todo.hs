{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.JSString (JSString)
import Data.JSString.Text (textFromJSString)
-- import Data.Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Generics
-- import SHSP
-- import HSP.XMLGenerator
-- import HSP.XML
-- import Network.Wai
import Network.HTTP.Types
import Servant.API
import Servant.Isomaniac
import Servant.Common.BaseUrl
-- import Servant.Docs
-- import Servant.Docs.Pandoc
-- import Text.Pandoc
import Language.Haskell.HSX.QQ (hsx)
import Web.ISO.HSX
import Web.ISO.Types
import Todo.Type.API
import Todo.Type.User
import Todo.Type.Todo
import Servant.Common.Req (Req(..))

instance HasIsomaniac api => HasIsomaniac (AuthToken :> api) where
  type Isomaniac (AuthToken :> api) = AuthToken -> Isomaniac api
  isomaniacWithRoute Proxy req url (AuthToken txt) =
    isomaniacWithRoute (Proxy :: Proxy api) newreq url
      where
        newreq = req { headers = [("X-Access-Token", txt)] ++ (headers req) }

userReq :: LoginUser -> ReqAction User
todoGetAll :: AuthToken -> Maybe OrderBy -> Maybe Completed -> ReqAction [Todo]
todoCreate :: AuthToken -> NewTodo -> ReqAction Todo
(userReq :<|> todoGetAll :<|> todoGet :<|> todoDelete :<|> todoUpdate :<|> todoCount :<|> todoCreate)  = isomaniac (Proxy :: Proxy API) host
    where host :: BaseUrl
          host = BaseUrl Http "localhost" 8000 "/"

{-
instance AsChild a Person where
    asChild p = [CDATA True ((firstName p) <> " " <> (lastName p) <> ", Age " <> (T.pack $ show (age p)))]
-}

data Model = Model
    { credentials :: Maybe User
    , todos       :: [Todo]
    , newTodo     :: T.Text
    }
data Action
    = Login LoginUser
    | Create
    | Created' Todo
    | UpdateCredentials User
    | UpdateAll [Todo]
    | NewTodoText (Maybe JSString)
      deriving Show

update' :: Action -> Model -> (Model, Maybe (ReqAction Action))
update' action model =
    case action of
      Login loginUser -> (model, Just (UpdateCredentials <$> userReq loginUser))
      Create -> case credentials model of
                  (Just user) -> (model, Just $ Created' <$> (todoCreate (token user) (NewTodo (newTodo model))))
                  Nothing -> (model, Nothing)
      Created' _ ->
          case credentials model of
                  (Just user) -> (model, Just (UpdateAll <$> todoGetAll (token user) Nothing Nothing))
                  Nothing -> (model, Nothing)
      UpdateCredentials user -> (model { credentials = Just user }, Just (UpdateAll <$> todoGetAll (token user) Nothing Nothing))
      UpdateAll ts -> (model { todos = ts }, Nothing)
      NewTodoText Nothing -> (model, Nothing)
      NewTodoText (Just jstr) -> (model { newTodo = textFromJSString jstr }, Nothing)

view' :: Model -> HTML Action
view' model =
          [hsx|
           <div>
            <h1>Todo!</h1>
            <% case credentials model of
                Nothing ->
                    <button onclick=(Login (LoginUser (UserName "stepcut") (Password "bar")))>Login</button>
                (Just user) ->
                     <div>
                       <p>Hello, <% toText (userName user) %>!</p>
                       <p>These are some things you should do.</p>
                       <ul>
                         <% map showTodo (todos model) %>
                       </ul>
                       <label for="newtodo">New Todo</label>
                       <input type="text" onchange=NewTodoText value="" />
                       <br />
                       <button onclick=Create>Create a new Todo</button>
                     </div>
             %>
           </div>
          |]

showTodo :: Todo -> HTML Action
showTodo Todo{..} =
    [hsx|
    <li>
     <dl>
      <dt>Created</dt>
      <dd><% show created %></dd>
     </dl>
     <dl>
      <dt>Completed</dt>
      <dd><% toText completed %></dd>
     </dl>
     <dl>
      <dt>Description</dt>
      <dd><% let (Description t) = description in t %></dd>
     </dl>
    </li>
     |]

todoMUV :: MUV Model Action (ReqAction Action)
todoMUV =
    MUV { model = Model { credentials = Nothing
                        , todos = []
                        , newTodo = ""
                        }
        , update = update'
        , view = view'
        }

main :: IO ()
main =
    do muv todoMUV Nothing
