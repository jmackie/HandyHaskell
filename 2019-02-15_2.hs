partition :: Foldable f => (a -> Either b c) -> f a -> ([b], [c])
partition p as = foldr (select p) ([],[]) as
{-# INLINE partition #-}
{-# SPECIALISE partition :: (a -> Either b c) -> [a] -> ([b], [c]) #-}

select :: (a -> Either b c) -> a -> ([b], [c]) -> ([b], [c])
select p a ~(bs, cs) =
  case p a of
    Left b  -> (b:bs, cs)
    Right c -> (bs, c:cs)
