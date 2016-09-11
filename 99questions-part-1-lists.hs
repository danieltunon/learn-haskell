module Part199Questions where
  -- 1 Find the last element of a list
  myLast :: [a] -> a
  myLast [] = error "Can't take end of empty list"
  myLast [x] = x
  myLast (_:xs) = myLast xs

  --  Find the last but 1 of a list
  myButLast :: [a] -> a
  myButLast [] = error "List must have 2 or more elements"
  myButLast [_] = error "List must have 2 or more elements"
  myButLast (x:[_]) = x
  myButLast (_:xs) = myButLast xs

  elementAt :: [a] -> Integer -> a
  elementAt [] _ = error "List isn't long enough"
  elementAt (x:_) 1 = x
  elementAt (_:xs) n = elementAt xs (n - 1)
