main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwords!"
          putStrLn $ "Yes, you said " ++ line' ++ " backwords!"

