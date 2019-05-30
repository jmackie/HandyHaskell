{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module ObjectLike where

import           Control.Applicative (liftA2)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.Proxy          (Proxy(..))
import           GHC.Generics
import           GHC.TypeLits        (Symbol, KnownSymbol, symbolVal)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

-- |
-- Wouldn't it be /swell/ if we could have the logic for mapping record fields
-- to json object keys all in one place? Currently you're options are pretty much:
--
--   * Write the instances by hand (explicit, but tedious)
--   * Put your mapping logic into aeson options (not always possible)
--   * Use the same keys and embrace the pain of -XDuplicateRecordFields
--
-- Here's what I want to write:

data MyRecord = MyRecord
  { _myRecordFoo :: Prop "foo" Int
  , _myRecordBar :: Prop "bar" Bool
  }
  deriving stock (Generic, Show)
  deriving FromJSON via ObjectLike MyRecord           -- <3 DerivingVia
  deriving ToJSON   via ObjectLike MyRecord

data MyProduct = MyProduct (Prop "x" Int) (Prop "y" Int)
  deriving stock (Generic, Show)
  deriving FromJSON via ObjectLike MyProduct
  deriving ToJSON   via ObjectLike MyProduct

-- |
-- This does bring a little bit of newtype awkwardness around @Prop@, but I
-- personally don't mind that so much. Lenses could also improve this.

main :: IO ()
main = do
  print $ decode @MyRecord "{\"foo\": 42, \"bar\": true}"
  print $ encode (MyRecord (Prop 42) (Prop True))

  print $ decode @MyProduct "{\"x\": 42, \"y\": 24}"
  print $ encode (MyProduct (Prop 42) (Prop 24))
  -- yay

-- |
-- @Prop@ lets us capture the keys associated with parts of a product type.

newtype Prop (key :: Symbol) a = Prop { unProp :: a }
  deriving stock (Generic)
  deriving newtype (Show)

-- |
-- @ObjectLike@ is our deriving via helper.

newtype ObjectLike a = ObjectLike a

instance (Generic a, FromObject (Rep a)) => FromJSON (ObjectLike a) where
  parseJSON value = do
    object <- parseJSON value
    x <- fromObject @(Rep a) object
    pure $ ObjectLike (to x)

instance (Generic a, ToObject (Rep a)) => ToJSON (ObjectLike a) where
  toJSON (ObjectLike a) = Object $ toObject (from a)

-- |
-- If a data type is equivalent to a bunch of @Prop@s then it has an instance
-- of @FromObject@.

class FromObject f where
  fromObject :: Object -> Parser (f p)

instance FromObject f => FromObject (M1 i c f) where
  fromObject object = M1 <$> fromObject @f object

instance (FromObject f, FromObject g) => FromObject (f :*: g) where
  fromObject obj = liftA2 (:*:) (fromObject obj) (fromObject obj)

instance (KnownSymbol key, FromJSON a) => FromObject (Rec0 (Prop key a)) where
  fromObject obj = K1 . Prop <$> obj .: key
    where key = Text.pack $ symbolVal (Proxy @key)

-- |
-- If a data type is equivalent to a bunch of @Prop@s then it has an instance
-- of @ToObject@.

class ToObject f where
  toObject :: f p -> Object

instance ToObject f => ToObject (M1 i c f) where
  toObject (M1 f) = toObject f

instance (ToObject f, ToObject g) => ToObject (f :*: g) where
  toObject (f :*: g) = toObject f <> toObject g

instance (KnownSymbol key, ToJSON a) => ToObject (Rec0 (Prop key a)) where
  toObject (K1 (Prop a)) = HashMap.singleton key (toJSON a)
    where key = Text.pack $ symbolVal (Proxy @key)
