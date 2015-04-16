main = do
  -- putStrLn "Hello, what's your name?"
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  x <- putStrLn ("Hey " ++ name ++ ", you rock!")
  return x

