module Utils where

infixl 5 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- |Generates all sublists of repetition of the given item.
repeatToN :: Int -> a -> [[a]]
repeatToN n a = replicate <$> [0 .. n] <*> pure a

-- |Get the first two elements of a tuple.
fst2of3 :: (a, b, c) -> (a, b)
fst2of3 (a, b, c) = (a, b)

-- |Compare tuples based on their third entry
compareThd :: Ord c => (a, b, c) -> (a, b, c) -> Ordering
compareThd (_, _, x1) (_, _, x2) = compare x1 x2
