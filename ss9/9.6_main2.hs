import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a','z') gen)
  gen2 <- newStdGen
  putStrLn $ take 20 (randomRs ('a','z') gen2)
  gen3 <- getStdGen
  putStrLn $ take 20 (randomRs ('a','z') gen3)
  gen4 <- getStdGen
  putStrLn $ take 20 (randomRs ('a','z') gen4)

