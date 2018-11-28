{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}  -- trust me ghc, you got this
{-# LANGUAGE FlexibleInstances      #-}

-- Natural numbers, promoted to the kind/type level via DataKinds
data Nat = Zero | Succ Nat


-- MultiParameterTypeClasses "declare" a type level function
class GreaterThan (x :: Nat) (y :: Nat) (b :: Bool) | x y -> b
--                                                    ^^^^^^^^
--                                            This is the function bit
--                                            `x` and `y` uniquely determine `b`


-- Instances are where we implement a type level function (above).
-- We match on types and return a type, much like we do with terms.
instance GreaterThan Zero Zero      False
instance GreaterThan (Succ x') Zero False
instance GreaterThan Zero (Succ y') True
-- Recursive case:
instance GreaterThan x' y' b => GreaterThan (Succ x') (Succ y') b


-- PRACTICAL EXAMPLE

newtype Vec (n :: Nat) a = Vec [a]

-- Passing an empty vector to this function is a compile time error.
safeHead :: GreaterThan Zero n True => Vec n a -> a
safeHead (Vec as) = head as

emptyVec :: Vec Zero Int
emptyVec = Vec []

singletonVec :: Vec (Succ Zero) Int
singletonVec = Vec [1]

-- safeHead singletonVec  <- fine
-- safeHead emptyVec      <- won't compile
