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
module Common where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
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
-- import Servant.Isomaniac
-- import Servant.Common.BaseUrl
-- import Servant.Docs
-- import Servant.Docs.Pandoc
-- import Text.Pandoc
-- import Language.Haskell.HSX.QQ (hsx)
-- import Web.ISO.HSX
-- import Web.ISO.Types

-- | Define a record to hold a 'Person'
data Person = Person
  { firstName :: T.Text
  , lastName  :: T.Text
  , age       :: Int
  } deriving (Show, Generic) -- for the JSON instance

-- | automatically derive a JSON instance for 'Person'
instance ToJSON Person
instance FromJSON Person

type PersonAPI = "persons" :> Get '[JSON] [Person]

personAPI :: Proxy PersonAPI
personAPI = Proxy
