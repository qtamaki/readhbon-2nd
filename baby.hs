doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x * 2

{-
for(i <- List(1,2,3))
  for(j <- List2(1,2,3)
    print 1
-}

