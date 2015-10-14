import Control.Monad

justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x

--fail :: (Monad m) => String -> m a
--fail msg = error msg

--fail _ = Nothing

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
            ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

