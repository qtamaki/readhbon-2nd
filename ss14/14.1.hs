
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "yyyyyy")

--           3, "xxxx" -> Int -> (Bool, "yyyyy")Gnh
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

