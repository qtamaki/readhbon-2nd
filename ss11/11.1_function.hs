instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- instance Functor ((->) r) where
--   fmap = (.)


