
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

mempty `mappend` x = x

x `mappend` mempty = x

(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

mempty = ""
mappend a b = a ++ b

"" ++ "abc" -> "abc"
"abc" ++ "" -> "abc"

mempty = 0
mappend a b = a + b

0 + 100 -> 100
100 + 0 -> 100

1 * 100 -> 100
100 * 1 -> 100

foldl 

foldr (++) "" ["a","b","c"]

("a" ++ "b") ++ "c" -> "abc"
"a" ++ ("b" ++ "c") -> "abc"

100 / 100 / 100 -> 0.01
100 / (100 / 100) -> 100



