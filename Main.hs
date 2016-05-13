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


instance AsChild a Person where
    asChild p = [CDATA True ((firstName p) <> " " <> (lastName p) <> ", Age " <> (T.pack $ show (age p)))]

persons :: ReqAction [Person]
persons = isomaniac personAPI host
    where host :: BaseUrl
          host = BaseUrl Http "localhost" 8000 "/"

data Action
    = Init
    | People [Person]
    | Refresh
    deriving Show

view' :: [Person] -> HTML Action
view' people =
    [hsx|
        <div>
         <p>These are the people in your neighborhood.</p>
         <ul>
          <% Prelude.map (\p -> <li><% p %></li>) people %>
         </ul>
         <button onclick=Refresh>Refresh</button>
        </div>
     |]

update' :: Action -> [Person] -> ([Person], Maybe (ReqAction Action))
update' action people =
    case action of
      Init -> (people, Just (People <$> persons))
      Refresh -> (people, Just (People <$> persons))
      (People newPeople) -> (newPeople, Nothing)

personMUV :: MUV [Person] Action (ReqAction Action)
personMUV =
    MUV { model = [Person "Joe" "Shmoe" 16]
        , update = update'
        , view = view'
        }

main :: IO ()
main =
    do muv personMUV Nothing -- (Just Init)
