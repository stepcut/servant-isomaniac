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
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import GHC.Generics
-- import SHSP
-- import HSP.XMLGenerator
-- import HSP.XML
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Servant
-- import Servant.Docs
-- import Servant.Docs.Pandoc
-- import Text.Pandoc
-- import Language.Haskell.HSX.QQ (hsx)

persons :: [Person]
persons =
  [ Person "Isaac"  "Newton"   372
  , Person "Albert" "Einstein" 136
  ]

type ServerAPI = PersonAPI :<|> Raw

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: Server ServerAPI
server = return persons
         :<|> serveClient
    where
      html = ("Content-Type", "text/html")
      plain = ("Content-Type", "text/plain")
      json = ("Content-Type", "application/json")
      serveClient req respond =
          case pathInfo req of
            ["index.html"] -> respond $ responseFile status200 [html] "Main.jsexe/index.html" Nothing
            ["rts.js"] -> respond $ responseFile status200 [json] "Main.jsexe/rts.js" Nothing
            ["out.js"] -> respond $ responseFile status200 [json] "Main.jsexe/out.js" Nothing
            ["lib.js"] -> respond $ responseFile status200 [json] "Main.jsexe/lib.js" Nothing
            ["runmain.js"] -> respond $ responseFile status200 [json] "Main.jsexe/runmain.js" Nothing
            _ -> respond $ responseLBS status200 [plain] "Not found."

-- | glue to create an 'Application'
app :: Application
app = serve serverAPI server

main :: IO ()
main = run 8000 app
