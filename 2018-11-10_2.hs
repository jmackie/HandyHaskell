{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
import GHC.Generics

class Record f where
    fieldNames' :: f p -> [String]

instance (Record a, Record b) => Record (a :*: b) where
    fieldNames' (a :*: b) = fieldNames' a <> fieldNames' b

instance Record a => Record (C1 c a) where
    fieldNames' (M1 a) = fieldNames' a

instance Record a => Record (D1 c a) where
    fieldNames' (M1 a) = fieldNames' a

instance Selector s => Record (S1 s a) where
    fieldNames' x = [selName x]

fieldNames :: (Generic a, Record (Rep a)) => a -> [String]
fieldNames = fieldNames' . from
