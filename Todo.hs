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
module Main where

import Common
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Map
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

-- user :: ReqBody '[JSON] LoginUser :> Post '[JSON] User -- ReqAction User
userReq = isomaniac (Proxy :: Proxy UserAPI) host
    where host :: BaseUrl
          host = BaseUrl Http "localhost" 8000 "/"

{-
instance AsChild a Person where
    asChild p = [CDATA True ((firstName p) <> " " <> (lastName p) <> ", Age " <> (T.pack $ show (age p)))]
-}

data Model = Model
    { credentials :: Maybe User
    }
data Action
    = Login LoginUser
    | UpdateCredentials User
      deriving Show

update' :: Action -> Model -> (Model, Maybe (ReqAction Action))
update' action model =
    case action of
      Login loginUser -> (model, Just (UpdateCredentials <$> userReq loginUser))
      UpdateCredentials user -> (model { credentials = Just user }, Nothing)

view' :: Model -> HTML Action
view' model =
          [hsx|
           <div>
            <h1>Todo!</h1>
           <% case credentials model of
                Nothing ->
                    <button onclick=(Login (LoginUser (UserName "foo") (Password "bar")))>Login</button>
                (Just user) ->
                    <p>Hello, <% toText (userName user) %>!</p>
            %>
            <p>These are some things you should do.</p>


           </div>
          |]

todoMUV :: MUV Model Action (ReqAction Action)
todoMUV =
    MUV { model = Model { credentials = Nothing
                        }
        , update = update'
        , view = view'
        }

main :: IO ()
main =
    do muv todoMUV Nothing


