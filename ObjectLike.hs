{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-
    One of the more tedious parts of writing a web application that consumes
    some json API is mapping json object keys to record selectors.

    For example: my type is @data Foo { foo :: Int, bar :: Int }@ and the API
    gives me something like @{"FooLol": 2, "BarHa": 2}@. This rules out deriving
    generic Aeson instances, and I'm left with a bunch of boring boilerplate to
    write.

    Go has quite a nice solution to this in the form of "struct tags", whereby
    json object keys can mapped right next to the type declaration itself. This
    is my attempt at bringing that same convenience to Haskell.
-}
module ObjectLike where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson (encodeToLazyText)
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText

import Control.Applicative (liftA2)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Generics
import GHC.TypeLits


data Person = Person
    { _name :: Prop "Name" T.Text
    , _age  :: Prop "Age"  Int
    }
    deriving stock Generic
    deriving anyclass ObjectLike


-- LENSES


name :: Functor f => (T.Text -> f T.Text) -> Person -> f Person
name f person =
    (\text -> person { _name = Prop text }) <$> f (unProp $ _name person)


age :: Functor f => (Int -> f Int) -> Person -> f Person
age f person =
    (\int -> person { _age = Prop int }) <$> f (unProp $ _age person)


-- OBJECT TYPES


class ObjectLike o where
    keys :: o -> [T.Text]
    default keys :: (Generic o, GObjectLike (Rep o)) => o -> [T.Text]
    keys = gkeys . from

    values :: o -> [Aeson.Value]
    default values :: (Generic o, GObjectLike (Rep o)) => o -> [Aeson.Value]
    values = gvalues . from


instance {-# OVERLAPPABLE #-} ObjectLike o => Show o where
    show = LazyText.unpack . Aeson.encodeToLazyText . Aeson.toJSON


instance {-# OVERLAPPABLE #-} ObjectLike o => Aeson.ToJSON o where
    toJSON = Aeson.object . liftA2 zip keys values


{- TODO: How to provide a FromJSON instance? -}


class GObjectLike o where
    gkeys   :: o x -> [T.Text]
    gvalues :: o x -> [Aeson.Value]


instance GObjectLike a => GObjectLike (M1 _1 _2 a) where
    gkeys   (M1 a) = gkeys a
    gvalues (M1 a) = gvalues a
    {-# INLINE gkeys   #-}
    {-# INLINE gvalues #-}


instance (GObjectLike a, GObjectLike b) => GObjectLike (a :*: b) where
    gkeys   (a :*: b) = gkeys a   <> gkeys b
    gvalues (a :*: b) = gvalues a <> gvalues b
    {-# INLINE gkeys   #-}
    {-# INLINE gvalues #-}


instance (KnownSymbol k, Aeson.ToJSON a) => GObjectLike (K1 _1 (Prop k a)) where
    gkeys   (K1 a) = [propKey a]
    gvalues (K1 a) = [Aeson.toJSON $ unProp a]
    {-# INLINE gkeys   #-}
    {-# INLINE gvalues #-}


newtype Prop (k :: Symbol) (a :: Type) = Prop { unProp :: a }


propKey :: forall k a . KnownSymbol k => Prop k a -> T.Text
propKey _ = T.pack . symbolVal $ Proxy @k


-- EXAMPLE


main :: IO ()
main = do
    print (keys person)
    print (values person)
    print (Aeson.toJSON person)
    print (Lens.view name person)
    print (Lens.set name "NAME" $ person)
  where
    person :: Person
    person = Person (Prop "test") (Prop 2)
