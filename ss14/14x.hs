import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: [DiffList] a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
    tell (DiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    tell (DiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    gcd' b (a `mod` b)
